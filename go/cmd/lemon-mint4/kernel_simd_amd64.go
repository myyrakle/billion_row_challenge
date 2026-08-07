//go:build goexperiment.simd && amd64

package main

import (
	"encoding/binary"
	"math/bits"
	"simd/archsimd"
	"unsafe"
)

const laneCount = 2

var (
	amd64PairWeights = [16]int8{10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1}
	amd64QuadWeights = [8]int16{100, 1, 100, 1, 100, 1, 100, 1}
)

func simdAvailable() bool {
	return archsimd.X86.AVX2()
}

func parseDigitsAMD64(word uint64, digitLen int, pairWeights archsimd.Int8x16, quadWeights archsimd.Int16x8) int32 {
	shifted := (word ^ uint64(0x3030303030303030)) << uint((8-digitLen)*8)
	var vector archsimd.Uint64x2
	digits := vector.SetElem(0, shifted).AsUint8x16()
	quads := digits.DotProductPairsSaturated(pairWeights).DotProductPairs(quadWeights)
	return quads.GetElem(0)*10_000 + quads.GetElem(1)
}

func processSIMD(data []byte, starts, ends *[laneCount]int, target *table) {
	semicolonVector := archsimd.BroadcastUint8x32(';')
	newlineVector := archsimd.BroadcastUint8x32('\n')
	pairWeights := archsimd.LoadInt8x16Array(&amd64PairWeights)
	quadWeights := archsimd.LoadInt16x8Array(&amd64QuadWeights)
	safeEnd := len(data) - 32

	for {
		if starts[0] >= ends[0] || starts[0] > safeEnd ||
			starts[1] >= ends[1] || starts[1] > safeEnd {
			return
		}

		{
			position := starts[0]
			chunk := archsimd.LoadUint8x32(data[position:])
			semicolonMask := chunk.Equal(semicolonVector).ToBits()
			newlineMask := chunk.Equal(newlineVector).ToBits()
			if semicolonMask == 0 || newlineMask == 0 {
				return
			}
			semicolon := bits.TrailingZeros32(semicolonMask)
			newline := bits.TrailingZeros32(newlineMask)
			if semicolon >= newline {
				return
			}
			digitLen := newline - semicolon - 1
			digitStart := position + semicolon + 1
			if digitLen < 1 || digitLen > 8 || digitStart+8 > len(data) {
				return
			}
			word := binary.LittleEndian.Uint64(data[digitStart : digitStart+8])
			key := stationKey(unsafe.Pointer(unsafe.SliceData(data[position:])), semicolon)
			updateSlot(&target[hashIndex(key)], parseDigitsAMD64(word, digitLen, pairWeights, quadWeights))
			starts[0] = position + newline + 1
		}

		{
			position := starts[1]
			chunk := archsimd.LoadUint8x32(data[position:])
			semicolonMask := chunk.Equal(semicolonVector).ToBits()
			newlineMask := chunk.Equal(newlineVector).ToBits()
			if semicolonMask == 0 || newlineMask == 0 {
				return
			}
			semicolon := bits.TrailingZeros32(semicolonMask)
			newline := bits.TrailingZeros32(newlineMask)
			if semicolon >= newline {
				return
			}
			digitLen := newline - semicolon - 1
			digitStart := position + semicolon + 1
			if digitLen < 1 || digitLen > 8 || digitStart+8 > len(data) {
				return
			}
			word := binary.LittleEndian.Uint64(data[digitStart : digitStart+8])
			key := stationKey(unsafe.Pointer(unsafe.SliceData(data[position:])), semicolon)
			updateSlot(&target[hashIndex(key)], parseDigitsAMD64(word, digitLen, pairWeights, quadWeights))
			starts[1] = position + newline + 1
		}
	}
}
