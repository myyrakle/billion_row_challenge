//go:build !amd64 && !arm64

package main

import "unsafe"

func simdAvailable() bool {
	return false
}

func processSIMD(starts, ends *uintptr, _ uintptr, _ *table) {
	startSlice := unsafe.Slice(starts, laneCount)
	endSlice := unsafe.Slice(ends, laneCount)
	copy(startSlice, endSlice)
}
