package main

import (
	"bytes"
	"flag"
	"fmt"
	"brc/internal/measurement"
	"math"
	"os"
	"runtime"
	"runtime/debug"
	"strconv"
	"sync"
	"syscall"
)

func chunkBounds(data []byte, workers int) []int {
	bounds := make([]int, workers+1)
	bounds[workers] = len(data)
	for worker := 1; worker < workers; worker++ {
		boundary := len(data) * worker / workers
		newline := bytes.IndexByte(data[boundary:], '\n')
		if newline < 0 {
			bounds[worker] = len(data)
		} else {
			bounds[worker] = boundary + newline + 1
		}
	}
	return bounds
}

func mergeAndFormat(tables []table) []byte {
	output := make([]byte, 0, 16*1024)
	for _, station := range stations {
		var merged slot
		for tableIndex := range tables {
			source := &tables[tableIndex][station.index]
			if source.count == 0 {
				continue
			}
			if merged.count == 0 {
				merged = *source
				continue
			}
			if source.min < merged.min {
				merged.min = source.min
			}
			if source.max > merged.max {
				merged.max = source.max
			}
			merged.sum += source.sum
			merged.count += source.count
		}
		if merged.count == 0 {
			continue
		}

		output = append(output, station.name...)
		output = append(output, '=')
		output = strconv.AppendInt(output, int64(merged.min), 10)
		output = append(output, ';')
		output = strconv.AppendInt(output, int64(merged.max), 10)
		output = append(output, ';')
		output = strconv.AppendInt(output, merged.sum/int64(merged.count), 10)
		output = append(output, '(')
		output = strconv.AppendInt(output, merged.sum, 10)
		output = append(output, '/')
		output = strconv.AppendUint(output, uint64(merged.count), 10)
		output = append(output, ')', '\n')
	}
	return output
}

func solutionWithWorkers(inputPath string, workers int) []byte {
	file, err := os.Open(inputPath)
	if err != nil {
		panic(err)
	}

	info, err := file.Stat()
	if err != nil {
		panic(err)
	}
	if info.Size() == 0 {
		return nil
	}

	data, err := syscall.Mmap(int(file.Fd()), 0, int(info.Size()), syscall.PROT_READ, syscall.MAP_SHARED)
	if err != nil {
		panic(err)
	}
	adviseMapped(data)

	if workers < 1 {
		workers = 1
	}
	bounds := chunkBounds(data, workers)
	tables := make([]table, workers)
	for tableIndex := range tables {
		for slotIndex := range hashSize {
			tables[tableIndex][slotIndex].min = math.MaxInt32
		}
	}

	var wait sync.WaitGroup
	wait.Add(workers - 1)
	for worker := range workers - 1 {
		go func() {
			defer wait.Done()
			processChunk(data, bounds[worker], bounds[worker+1], &tables[worker])
		}()
	}
	last := workers - 1
	processChunk(data, bounds[last], bounds[last+1], &tables[last])
	wait.Wait()

	runtime.KeepAlive(data)
	runtime.KeepAlive(tables)
	return mergeAndFormat(tables)
}
func solution(inputPath string) []byte {
	return solutionWithWorkers(inputPath, defaultWorkerCount())
}

func defaultWorkerCount() int {
	workers := runtime.NumCPU()
	if runtime.GOOS == "darwin" {
		return workers * 4
	}
	return workers
}

func main() {
	workerFlag := flag.Int("workers", 0, "worker count (default: platform-tuned)")
	flag.Parse()
	expected, err := os.ReadFile(measurement.OUTPUT_PATH)
	if err != nil {
		panic(err)
	}

	workers := *workerFlag
	if workers < 1 {
		workers = defaultWorkerCount()
	}
	runtime.GOMAXPROCS(workers)
	debug.SetGCPercent(-1)
	debug.SetMemoryLimit(math.MaxInt64)

	timer := measurement.NewTimer()
	got := solutionWithWorkers(measurement.MEASUREMENTS_PATH, workers)
	fmt.Printf("Elapsed: %fms\n", timer.ElapsedAsMilliseconds())
	if !bytes.Equal(expected, got) {
		os.Stdout.Write(got)
		panic("output mismatch")
	}
	fmt.Println("Matched")
}
