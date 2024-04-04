package wyhash

import (
	"math/bits"
	"unsafe"
)

func _wymix(A uint64, B uint64) uint64 {
	B, A = bits.Mul64(A, B)
	return A ^ B
}

func _wyr8(p *uint8) uint64 {
	var pp = (*[8]uint8)(unsafe.Pointer(p))
	return uint64(pp[0]) | uint64(pp[1])<<8 | uint64(pp[2])<<16 | uint64(pp[3])<<24 | uint64(pp[4])<<32 | uint64(pp[5])<<40 | uint64(pp[6])<<48 | uint64(pp[7])<<56
}

func _wyr4(p *uint8) uint64 {
	var pp = (*[4]uint8)(unsafe.Pointer(p))
	return uint64(uint32(pp[0]) | uint32(pp[1])<<8 | uint32(pp[2])<<16 | uint32(pp[3])<<24)
}

func _wyr3(p *uint8, k uintptr) uint64 {
	// (((uint64_t)p[0]) << 16) | (((uint64_t)p[k >> 1]) << 8) | p[k - 1];
	return (uint64(*p) << 16) |
		(uint64(*(*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p)) + k>>1))) << 8) |
		(uint64(*(*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p)) + k - 1))))
}

var _wyp = [4]uint64{0xa0761d6478bd642f, 0xe7037ed1a0b428db, 0x8ebc6af09c88c6e3, 0x589965cc75374cc3}

func wyhash(key unsafe.Pointer, len uintptr, seed uint64, secret *[4]uint64) uint64 {
	var p *uint8 = (*uint8)(key)
	seed ^= secret[0]
	var a, b uint64
	if len <= 16 {
		if len >= 4 {
			a = (_wyr4(p) << 32) | _wyr4((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+((len>>3)<<2))))
			b = (_wyr4((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+len-4))) << 32) | _wyr4((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+len-4-((len>>3)<<2))))
		} else if len > 0 {
			a = _wyr3(p, len)
			b = 0
		}
	} else {
		var i = len
		if i > 48 {
			var see1, see2 uint64 = seed, seed
			for {
				seed = _wymix(_wyr8(p)^secret[1], _wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+8)))^seed)
				see1 = _wymix(_wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+16)))^secret[2], _wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+24)))^see1)
				see2 = _wymix(_wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+32)))^secret[3], _wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+40)))^see2)
				p = (*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p)) + 48))
				i -= 48
				if i <= 48 {
					break
				}
			}
			seed ^= see1 ^ see2
		}
		for i > 16 {
			seed = _wymix(_wyr8(p)^secret[1], _wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p))+8)))^seed)
			i -= 16
			p = (*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p)) + 16))
		}
		a = _wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p)) + i - 16)))
		b = _wyr8((*uint8)(unsafe.Pointer(uintptr(unsafe.Pointer(p)) + i - 8)))
	}
	return _wymix(secret[1]^uint64(len), _wymix(a^secret[1], b^seed))
}

func wyrand(seed *uint64) uint64 {
	*seed += 0xa0761d6478bd642f
	return _wymix(*seed, *seed^0xe7037ed1a0b428db)
}

func WYRAND(seed *uint64) uint64 { return wyrand(seed) }

func WYHASH_RAW(key unsafe.Pointer, len uintptr, seed uint64) uint64 {
	return wyhash(key, len, seed, &_wyp)
}
