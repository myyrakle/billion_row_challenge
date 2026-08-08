package main

import (
	"bytes"
	"runtime"
	"unsafe"
)

const hashSize = 1 << 9

type slot struct {
	sum   int64
	count uint32
	min   int32
	max   int32
	_     uint32
}

type table [hashSize]slot

func hashIndex(key uint64) uint64 {
	return key * hashMultiplier >> 55
}

func stationKey(position unsafe.Pointer, nameLen int) uint64 {
	first := uint64(*(*uint32)(position))
	return first | uint64(uint32(nameLen))<<32
}

func updateSlot(target *slot, value int32) {
	if target.count == 0 {
		target.sum = int64(value)
		target.count = 1
		target.min = value
		target.max = value
		return
	}
	if value < target.min {
		target.min = value
	}
	if value > target.max {
		target.max = value
	}
	target.sum += int64(value)
	target.count++
}

func processScalar(data []byte, start, end int, target *table) {
	for start < end {
		recordStart := start
		for start < end && data[start] != ';' {
			start++
		}
		if start >= end {
			return
		}
		nameLen := start - recordStart
		start++

		var value int32
		for start < end && data[start] != '\n' {
			value = value*10 + int32(data[start]-'0')
			start++
		}
		if start < end {
			start++
		}

		key := stationKey(unsafe.Pointer(unsafe.SliceData(data[recordStart:])), nameLen)
		updateSlot(&target[hashIndex(key)], value)
	}
}

func splitLanes(data []byte, start, end int) (bounds [laneCount + 1]int) {
	bounds[0] = start
	bounds[laneCount] = end
	span := end - start
	for lane := 1; lane < laneCount; lane++ {
		boundary := start + span*lane/laneCount
		if boundary >= end {
			bounds[lane] = end
			continue
		}
		newline := bytes.IndexByte(data[boundary:end], '\n')
		if newline < 0 {
			bounds[lane] = end
		} else {
			bounds[lane] = boundary + newline + 1
		}
	}
	return bounds
}

func processChunk(data []byte, start, end int, target *table) {
	if end-start < 4096 || !simdAvailable() || len(data) < 32 {
		processScalar(data, start, end, target)
		return
	}

	bounds := splitLanes(data, start, end)
	base := uintptr(unsafe.Pointer(unsafe.SliceData(data)))
	var starts, ends [laneCount]uintptr
	for lane := range laneCount {
		starts[lane] = base + uintptr(bounds[lane])
		ends[lane] = base + uintptr(bounds[lane+1])
	}

	processSIMD(&starts[0], &ends[0], base+uintptr(len(data)-32), target)
	for lane := range laneCount {
		if starts[lane] < ends[lane] {
			processScalar(data, int(starts[lane]-base), int(ends[lane]-base), target)
		}
	}
	runtime.KeepAlive(data)
	runtime.KeepAlive(target)
}
