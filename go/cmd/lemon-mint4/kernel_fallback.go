//go:build !goexperiment.simd || (!amd64 && !arm64)

package main

const laneCount = 3

func simdAvailable() bool {
	return false
}

func processSIMD(_ []byte, starts, ends *[laneCount]int, _ *table) {
	*starts = *ends
}
