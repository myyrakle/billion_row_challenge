//go:build goexperiment.simd && (amd64 || arm64)

package main

func parseDigitsSWAR(word uint64, digitLen int) int32 {
	const (
		asciiZeros      = uint64(0x3030303030303030)
		pairMask        = uint64(0x000000ff000000ff)
		lowPairWeights  = uint64(0x000f424000000064)
		highPairWeights = uint64(0x0000271000000001)
	)

	digits := (word ^ asciiZeros) << uint((8-digitLen)*8)
	pairs := digits*10 + digits>>8
	low := (pairs & pairMask) * lowPairWeights
	high := ((pairs >> 16) & pairMask) * highPairWeights
	return int32((low + high) >> 32)
}
