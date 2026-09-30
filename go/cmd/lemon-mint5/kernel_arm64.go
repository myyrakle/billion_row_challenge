package main

// Go's arm64 runtime and standard library use Advanced SIMD instructions on
// every supported arm64 target, so no additional optional feature is required.
func simdAvailable() bool {
	return true
}

//go:noescape
func processSIMD(starts, ends *uintptr, safeEnd uintptr, target *table)
