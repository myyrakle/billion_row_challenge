package main

var avx2Available = detectAVX2()

func simdAvailable() bool {
	return avx2Available
}

func detectAVX2() bool {
	maximum, _, _, _ := cpuid(0, 0)
	if maximum < 1 {
		return false
	}
	_, _, features, _ := cpuid(1, 0)
	const avxAndOSXSAVE = 1<<28 | 1<<27
	if features&avxAndOSXSAVE != avxAndOSXSAVE {
		return false
	}
	xcr0, _ := xgetbv()
	if xcr0&0x6 != 0x6 || maximum < 7 {
		return false
	}
	_, extended, _, _ := cpuid(7, 0)
	return extended&(1<<5) != 0
}

//go:noescape
func processSIMD(starts, ends *uintptr, safeEnd uintptr, target *table)

//go:noescape
func cpuid(leaf, subleaf uint32) (eax, ebx, ecx, edx uint32)

//go:noescape
func xgetbv() (eax, edx uint32)
