//go:build goexperiment.simd && arm64

package main

import (
	"encoding/binary"
	"simd/archsimd"
	"unsafe"
)

const laneCount = 3

var arm64DelimiterIndexes = [16]uint8{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}

func simdAvailable() bool {
	return true
}

func firstDelimiter(chunk, delimiter, indexes, notFound archsimd.Uint8x16) int {
	return int(indexes.IfElse(chunk.Equal(delimiter), notFound).ReduceMin())
}

func delimiterOffsets(data []byte, position int, semicolonVector, newlineVector, indexes, notFound archsimd.Uint8x16) (int, int, bool) {
	low := archsimd.LoadUint8x16(data[position:])
	high := archsimd.LoadUint8x16(data[position+16:])

	semicolon := firstDelimiter(low, semicolonVector, indexes, notFound)
	if semicolon == 16 {
		semicolon = firstDelimiter(high, semicolonVector, indexes, notFound)
		if semicolon == 16 {
			return 0, 0, false
		}
		semicolon += 16
	}

	newline := firstDelimiter(low, newlineVector, indexes, notFound)
	if newline == 16 {
		newline = firstDelimiter(high, newlineVector, indexes, notFound)
		if newline == 16 {
			return 0, 0, false
		}
		newline += 16
	}
	return semicolon, newline, semicolon < newline
}

func processSIMD(data []byte, starts, ends *[laneCount]int, target *table) {
	semicolonVector := archsimd.BroadcastUint8x16(';')
	newlineVector := archsimd.BroadcastUint8x16('\n')
	indexes := archsimd.LoadUint8x16Array(&arm64DelimiterIndexes)
	notFound := archsimd.BroadcastUint8x16(16)
	safeEnd := len(data) - 32

	for {
		if starts[0] >= ends[0] || starts[0] > safeEnd ||
			starts[1] >= ends[1] || starts[1] > safeEnd ||
			starts[2] >= ends[2] || starts[2] > safeEnd {
			return
		}

		{
			position := starts[0]
			semicolon, newline, ok := delimiterOffsets(
				data, position, semicolonVector, newlineVector, indexes, notFound,
			)
			if !ok {
				return
			}
			digitLen := newline - semicolon - 1
			digitStart := position + semicolon + 1
			if digitLen < 1 || digitLen > 8 || digitStart+8 > len(data) {
				return
			}
			word := binary.LittleEndian.Uint64(data[digitStart : digitStart+8])
			key := stationKey(unsafe.Pointer(unsafe.SliceData(data[position:])), semicolon)
			updateSlot(&target[hashIndex(key)], parseDigitsSWAR(word, digitLen))
			starts[0] = position + newline + 1
		}

		{
			position := starts[1]
			semicolon, newline, ok := delimiterOffsets(
				data, position, semicolonVector, newlineVector, indexes, notFound,
			)
			if !ok {
				return
			}
			digitLen := newline - semicolon - 1
			digitStart := position + semicolon + 1
			if digitLen < 1 || digitLen > 8 || digitStart+8 > len(data) {
				return
			}
			word := binary.LittleEndian.Uint64(data[digitStart : digitStart+8])
			key := stationKey(unsafe.Pointer(unsafe.SliceData(data[position:])), semicolon)
			updateSlot(&target[hashIndex(key)], parseDigitsSWAR(word, digitLen))
			starts[1] = position + newline + 1
		}

		{
			position := starts[2]
			semicolon, newline, ok := delimiterOffsets(
				data, position, semicolonVector, newlineVector, indexes, notFound,
			)
			if !ok {
				return
			}
			digitLen := newline - semicolon - 1
			digitStart := position + semicolon + 1
			if digitLen < 1 || digitLen > 8 || digitStart+8 > len(data) {
				return
			}
			word := binary.LittleEndian.Uint64(data[digitStart : digitStart+8])
			key := stationKey(unsafe.Pointer(unsafe.SliceData(data[position:])), semicolon)
			updateSlot(&target[hashIndex(key)], parseDigitsSWAR(word, digitLen))
			starts[2] = position + newline + 1
		}
	}
}
