package main

import (
	"brc/cmd/lemon-mint1/wyhash"
	"testing"
	"unsafe"
)

func Test_PHF(t *testing.T) {
	var set = map[uint64]struct{}{}
	const hashseed = 15385993594274405086
	for _, city := range city_names {
		hash := wyhash.WYHASH_RAW(unsafe.Pointer(unsafe.StringData(city)), uintptr((len(city))), hashseed)
		hash = hash & hash_mask

		if _, ok := set[hash]; ok {
			t.Errorf("Collision detected for city '%s' with hash %d", city, hash)
		} else {
			set[hash] = struct{}{}
		}
	}
}
