package main

import (
	"bytes"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"testing"
	"unsafe"
)

type referenceSlot struct {
	sum   int64
	count int64
	min   int32
	max   int32
}

func appendReference(output []byte, values map[string]referenceSlot) []byte {
	for _, station := range stations {
		value, exists := values[station.name]
		if !exists {
			continue
		}
		output = append(output, station.name...)
		output = append(output, '=')
		output = strconv.AppendInt(output, int64(value.min), 10)
		output = append(output, ';')
		output = strconv.AppendInt(output, int64(value.max), 10)
		output = append(output, ';')
		output = strconv.AppendInt(output, value.sum/value.count, 10)
		output = append(output, '(')
		output = strconv.AppendInt(output, value.sum, 10)
		output = append(output, '/')
		output = strconv.AppendInt(output, value.count, 10)
		output = append(output, ')', '\n')
	}
	return output
}

func makeFixture(t *testing.T, repetitions int) (string, []byte) {
	t.Helper()
	boundaryValues := [...]int32{0, 1, 9, 10, 99, 100, 999, 1000, 9999, 10000, 9_999_999, 10_000_000, 49_999_999}
	reference := make(map[string]referenceSlot, len(stations))
	var input bytes.Buffer

	for repetition := range repetitions {
		for stationOffset := range len(stations) {
			station := stations[(stationOffset+repetition)%len(stations)]
			value := boundaryValues[(stationOffset*7+repetition)%len(boundaryValues)]
			input.WriteString(station.name)
			input.WriteByte(';')
			input.WriteString(strconv.FormatInt(int64(value), 10))
			input.WriteByte('\n')

			current, exists := reference[station.name]
			if !exists {
				current.min = math.MaxInt32
			}
			current.sum += int64(value)
			current.count++
			if value < current.min {
				current.min = value
			}
			if value > current.max {
				current.max = value
			}
			reference[station.name] = current
		}
	}

	path := filepath.Join(t.TempDir(), "measurements.txt")
	if err := os.WriteFile(path, input.Bytes(), 0o644); err != nil {
		t.Fatal(err)
	}
	return path, appendReference(nil, reference)
}

func TestPerfectHash(t *testing.T) {
	seen := [hashSize]bool{}
	for _, station := range stations {
		key := stationKey(unsafeStringData(station.name), len(station.name))
		index := hashIndex(key)
		if index != uint64(station.index) {
			t.Fatalf("%q generated index %d, hash returned %d", station.name, station.index, index)
		}
		if seen[index] {
			t.Fatalf("hash collision at slot %d", index)
		}
		seen[index] = true
	}
}

func unsafeStringData(value string) unsafe.Pointer {
	return unsafe.Pointer(unsafe.StringData(value))
}

func TestSolutionGeneratedSmall(t *testing.T) {
	path, expected := makeFixture(t, 32)
	for _, workers := range [...]int{1, 2, 3, 4} {
		got := solutionWithWorkers(path, workers)
		if !bytes.Equal(got, expected) {
			t.Fatalf("workers=%d output mismatch\nwant:\n%s\ngot:\n%s", workers, expected, got)
		}
	}
}

func TestSIMDKernelConsumesRecords(t *testing.T) {
	if !simdAvailable() {
		t.Skip("SIMD unavailable")
	}
	path, expected := makeFixture(t, 32)
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}

	bounds := splitLanes(data, 0, len(data))
	base := uintptr(unsafe.Pointer(unsafe.SliceData(data)))
	var starts, initial, ends [laneCount]uintptr
	for lane := range laneCount {
		starts[lane] = base + uintptr(bounds[lane])
		initial[lane] = starts[lane]
		ends[lane] = base + uintptr(bounds[lane+1])
	}
	var target table
	for index := range target {
		target[index].min = math.MaxInt32
	}

	processSIMD(&starts[0], &ends[0], base+uintptr(len(data)-32), &target)
	for lane := range laneCount {
		if starts[lane] <= initial[lane] {
			t.Fatalf("lane %d did not advance", lane)
		}
		if starts[lane] > ends[lane] {
			t.Fatalf("lane %d advanced past its end", lane)
		}
		if starts[lane] < ends[lane] {
			processScalar(data, int(starts[lane]-base), int(ends[lane]-base), &target)
		}
	}
	runtime.KeepAlive(data)

	got := mergeAndFormat([]table{target})
	if !bytes.Equal(got, expected) {
		t.Fatalf("direct SIMD output mismatch\nwant:\n%s\ngot:\n%s", expected, got)
	}
}

func TestSolutionScalarTail(t *testing.T) {
	path, expected := makeFixture(t, 1)
	got := solutionWithWorkers(path, 3)
	if !bytes.Equal(got, expected) {
		t.Fatalf("scalar-tail output mismatch\nwant:\n%s\ngot:\n%s", expected, got)
	}
}
