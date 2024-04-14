package main

import (
	"brc/cmd/lemon-mint1/mmap"
	"brc/cmd/lemon-mint1/wyhash"
	"brc/internal/measurement"
	"bytes"
	"fmt"
	"math"
	"os"
	"runtime"
	"runtime/debug"
	"sort"
	"sync"
	"syscall"
	"unsafe"
)

var cores = runtime.NumCPU()
var pagesize = syscall.Getpagesize()

// Perfect Hash Function
const hash_space = 1 << 9
const hash_mask = hash_space - 1
const hash_seed = uint64(15385993594274405086)

type hashtable struct {
	buckets [hash_space]bucket
}

type bucket struct {
	// maximize L1 cache efficiency
	Len byte
	Key [31]byte

	Value stats
}

type stats struct {
	min, max, sum int64
	count         int64
}

func fastString(b []byte) string { return unsafe.String(unsafe.SliceData(b), len(b)) }

func ht_index(b *byte, len int64) uint64 {
	return wyhash.WYHASH_RAW(unsafe.Pointer(b), uintptr(len), hash_seed) & hash_mask
}

func chunk(view []byte, n int64) (bounds []int64) {
	bounds = make([]int64, n+1)
	bounds[n] = int64(len(view))

	block_size := int64(len(view)) / int64(n)
	var offset int64

	// Assume the view is large enough not to cause an index out of bounds error.
	for i := int64(1); i < n; i++ {
		offset = block_size * i
		newline_index := bytes.IndexByte(view[offset:], '\n')
		if newline_index == -1 {
			panic("Invalid View")
		}
		offset += int64(newline_index + 1)
		bounds[i] = offset
	}

	return bounds
}

func parseIntNoCheck(b []byte) (out int64) {
	var neg bool
	if b[0] == '-' {
		neg = true
		b = b[1:]
	} else if b[0] == '+' {
		neg = false
		b = b[1:]
	}

	var exp int64 = 1
	for i := len(b) - 1; i >= 0; i-- {
		out += int64((b[i] - '0')) * exp
		exp *= 10
	}

	if neg {
		out = -out
	}
	return
}

func processChunk(ht *hashtable, view []byte, wg *sync.WaitGroup) {
	defer wg.Done()
	for i := range ht.buckets {
		ht.buckets[i].Value.max = math.MinInt64
		ht.buckets[i].Value.min = math.MaxInt64
	}

	index := int64(0)
	for {
		city_index := bytes.IndexByte(view[index:], ';')
		if city_index == -1 {
			return
		}

		city_bytes := view[index : index+int64(city_index)]
		idx := ht_index(unsafe.SliceData(city_bytes), int64(city_index))

		temp_index := bytes.IndexByte(view[index+int64(city_index)+1:], '\n')
		temp_bytes := view[index+int64(city_index)+1 : index+int64(city_index)+1+int64(temp_index)]
		temp := parseIntNoCheck(temp_bytes)

		if ht.buckets[idx].Len == 0 {
			// Copy Key
			ht.buckets[idx].Len = byte(city_index)
			copy(ht.buckets[idx].Key[:], city_bytes)
		}

		if temp < ht.buckets[idx].Value.min {
			ht.buckets[idx].Value.min = temp
		}

		if temp > ht.buckets[idx].Value.max {
			ht.buckets[idx].Value.max = temp
		}

		ht.buckets[idx].Value.sum += temp
		ht.buckets[idx].Value.count++

		index += int64(city_index) + 1 + int64(temp_index) + 1
	}
}

func solution(inputPath string) []byte {
	buffer := make([]byte, 0, pagesize)

	// Open File
	finfo, err := os.Stat(inputPath)
	if err != nil {
		panic(err)
	}

	fd, err := syscall.Open(inputPath, syscall.O_RDONLY, 0)
	if err != nil {
		panic(err)
	}
	defer syscall.Close(fd)

	view, err := mmap.Map(uintptr(fd), 0, int(finfo.Size()), mmap.PROT_READ, mmap.MAP_SHARED)
	if err != nil {
		panic(err)
	}
	defer mmap.UnMap(view)

	var wg sync.WaitGroup
	var hashtables []hashtable = make([]hashtable, int64(cores))
	wg.Add(cores)

	bounds := chunk(view, int64(cores))
	for i := range int64(cores) - 1 {
		chunk := view[bounds[i]:bounds[i+1]]
		go processChunk(&hashtables[i], chunk, &wg)
	}
	processChunk(&hashtables[cores-1], view[bounds[cores-1]:bounds[cores]], &wg)
	wg.Wait()

	stationStats := make(map[string]*stats)
	stations := make([]string, 0, 128)

	// Merge Results (TODO: Optimize)
	for i := range hashtables {
		for j := range hashtables[i].buckets {
			if hashtables[i].buckets[j].Len > 0 {
				key := fastString(hashtables[i].buckets[j].Key[:hashtables[i].buckets[j].Len])
				stats := stationStats[key]
				if stats == nil {
					stationStats[key] = &hashtables[i].buckets[j].Value
					stations = append(stations, key)
					continue
				}
				if hashtables[i].buckets[j].Value.min < stats.min {
					stats.min = hashtables[i].buckets[j].Value.min
				}
				if hashtables[i].buckets[j].Value.max > stats.max {
					stats.max = hashtables[i].buckets[j].Value.max
				}
				stats.sum += hashtables[i].buckets[j].Value.sum
				stats.count += hashtables[i].buckets[j].Value.count
			}
		}
	}
	sort.Strings(stations)

	for _, station := range stations {
		s := stationStats[station]
		mean := s.sum / s.count
		buffer = fmt.Appendf(buffer, "%s=%d;%d;%d(%d/%d)\n", station, s.min, s.max, mean, s.sum, s.count)
	}

	runtime.KeepAlive(&hashtables) // Memory Lifetime

	return buffer
}

func main() {
	expect_output, err := os.ReadFile(measurement.OUTPUT_PATH)
	if err != nil {
		panic(err)
	}

	debug.SetGCPercent(-1)
	debug.SetMemoryLimit(math.MaxInt64) // Disable GC (not a big difference, but still effective)

	timer := measurement.NewTimer()
	got := solution(measurement.MEASUREMENTS_PATH)
	fmt.Printf("Elapsed: %fms\n", timer.ElapsedAsMilliseconds())

	if string(expect_output) != string(got) {
		fmt.Println("Expect:")
		os.Stdout.Write(expect_output)
		fmt.Println("Got:")
		os.Stdout.Write(got)
		panic("Not matched")
	} else {
		fmt.Println("Matched")
	}
}
