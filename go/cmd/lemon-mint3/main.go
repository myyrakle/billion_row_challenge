package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"reflect"
	"runtime"
	"runtime/debug"
	"runtime/pprof"
	"runtime/trace"
	"sort"
	"strings"
	"sync"
	"syscall"
	"time"
	"unsafe"
)

type Timer struct {
	start int64
}

func NewTimer() Timer {
	return Timer{
		start: time.Now().UnixNano(),
	}
}

func (t *Timer) Elapsed() time.Duration {
	return time.Duration(time.Now().UnixNano() - t.start)
}

func (t *Timer) ElapsedAsSeconds() float64 {
	return t.Elapsed().Seconds()
}

func (t *Timer) ElapsedAsMilliseconds() float64 {
	return t.Elapsed().Seconds() * 1000
}

const (
	PROT_READ  = syscall.PROT_READ
	PROT_WRITE = syscall.PROT_WRITE

	MAP_SHARED  = syscall.MAP_SHARED
	MAP_PRIVATE = syscall.MAP_PRIVATE
)

func Map(fd uintptr, offset int, len int, prot int, flags int) ([]byte, error) {
	return syscall.Mmap(int(fd), int64(offset), len, prot, flags)
}

func UnMap(b []byte) error {
	return syscall.Munmap(b)
}

const OUTPUT_PATH = "outputs.txt"
const MEASUREMENTS_PATH = "measurements.txt"

var threads = runtime.NumCPU() * 3 / 2
var pagesize = syscall.Getpagesize()

const hash_space = 1 << 8
const hash_mask = hash_space - 1

type hashtable struct {
	buckets [hash_space]bucket
}

type bucket struct {
	Key   [31]byte // 31
	Len   byte     // 32
	Value stats    // 64
}

type stats struct {
	min, max, sum int64
	count         int64
}

func fastString(b []byte) string { return *(*string)(unsafe.Pointer(&b)) }

func ht_index(b unsafe.Pointer, _len int64) uint64 {
	if _len > 8 {
		_len = 8
	}
	var x0 uint64 = *(*uint64)(b) << ((8 - _len) * 8)
	x0 += 7048676786887195564
	x0 = (x0 ^ (x0 >> 30)) * 0xbf58476d1ce4e5b9
	x0 = (x0 ^ (x0 >> 27)) * 0x94d049bb133111eb
	return (x0 ^ (x0 >> 31)) & hash_mask
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

const msk0 uint64 = 0b0011000000110000001100000011000000110000001100000011000000110000
const msk1 uint64 = 0b0000000000000000000000001111111100000000000000000000000011111111
const exp0 uint64 = 0b0000000000000000001001110001000000000000000000000000000000000001
const exp1 uint64 = 0b0000000000001111010000100100000000000000000000000000000001100100

//go:nosplit
func processChunk(ht *hashtable, view []byte, wg *sync.WaitGroup) {
	for i := range ht.buckets {
		ht.buckets[i].Value.max = 0
		ht.buckets[i].Value.min = math.MaxInt64
	}

	index := int64(0)
	for {
		city_index := bytes.IndexByte(view[index:], ';')
		if city_index == -1 {
			wg.Done()
			return
		}

		city_bytes := view[index : index+int64(city_index)]
		idx := ht_index(unsafe.Pointer((*reflect.SliceHeader)(unsafe.Pointer(&city_bytes)).Data), int64(city_index))

		temp_index := bytes.IndexByte(view[index+int64(city_index)+1:], '\n')
		temp_bytes := view[index+int64(city_index)+1 : index+int64(city_index)+1+int64(temp_index)]
		var val uint64 = *(*uint64)(unsafe.Pointer((*reflect.SliceHeader)(unsafe.Pointer(&temp_bytes)).Data))
		val ^= msk0
		val = (val << ((8 - len(temp_bytes)) * 8))
		val = (val * 10) + (val >> 8)
		val = (((val & msk1) * exp1) + (((val >> 16) & msk1) * exp0)) >> 32
		temp := int64(val)

		if ht.buckets[idx].Key[0] == 0 {
			ht.buckets[idx].Len = byte(city_index)
			copy(ht.buckets[idx].Key[:], city_bytes)
		}

		ht.buckets[idx].Value.min = ht.buckets[idx].Value.min + ((temp - ht.buckets[idx].Value.min) & ((temp - ht.buckets[idx].Value.min) >> 63))
		ht.buckets[idx].Value.max = ht.buckets[idx].Value.max - ((ht.buckets[idx].Value.max - temp) & ((ht.buckets[idx].Value.max - temp) >> 63))
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

	view, err := Map(uintptr(fd), 0, int(finfo.Size()), PROT_READ, MAP_SHARED)
	if err != nil {
		panic(err)
	}

	var wg sync.WaitGroup
	var hashtables []hashtable = make([]hashtable, int64(threads))
	wg.Add(threads)

	bounds := chunk(view, int64(threads))
	for i := int64(0); i < int64(threads); i++ {
		chunk := view[bounds[i]:bounds[i+1]]
		go processChunk(&hashtables[i], chunk, &wg)
	}
	wg.Wait()

	stationStats := make(map[string]*stats)
	stations := make([]string, 0, 128)

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
		buffer = append(buffer, fmt.Sprintf("%s=%d;%d;%d(%d/%d)\n", station, s.min, s.max, mean, s.sum, s.count)...)
	}

	runtime.KeepAlive(&hashtables) // Memory Lifetime

	return buffer
}

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to `file`")
var traceprofile = flag.String("traceprofile", "", "write trace profile to `file`")
var memprofile = flag.String("memprofile", "", "write memory profile to `file`")

func main() {
	flag.Parse()
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal("could not create CPU profile: ", err)
		}
		defer f.Close()

		if err := pprof.StartCPUProfile(f); err != nil {
			log.Fatal("could not start CPU profile: ", err)
		}
		defer pprof.StopCPUProfile()
	}

	if *traceprofile != "" {
		f, err := os.Create(*traceprofile)
		if err != nil {
			log.Fatal("could not create Trace profile: ", err)
		}
		defer f.Close()

		if err := trace.Start(f); err != nil {
			log.Fatal("could not start Trace profile: ", err)
		}
		defer trace.Stop()
	}

	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal("could not create Memory profile: ", err)
		}
		defer f.Close()

		if err := pprof.WriteHeapProfile(f); err != nil {
			log.Fatal("could not write Memory profile: ", err)
		}
	}

	expect_output, err := os.ReadFile(OUTPUT_PATH)
	if err != nil {
		panic(err)
	}

	runtime.GOMAXPROCS(threads)
	debug.SetGCPercent(-1)

	timer := NewTimer()
	got := solution(MEASUREMENTS_PATH)
	fmt.Printf("Elapsed: %fms\n", timer.ElapsedAsMilliseconds())

	if strings.TrimSpace(string(expect_output)) != strings.TrimSpace(string(got)) {
		fmt.Println("Expect:")
		os.Stdout.Write(expect_output)
		fmt.Println("Got:")
		os.Stdout.Write(got)
		panic("Not matched")
	} else {
		fmt.Println("Matched")
	}
}
