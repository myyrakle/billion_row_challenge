//go:build !unix && !linux && !darwin && !freebsd && !openbsd
// +build !unix,!linux,!darwin,!freebsd,!openbsd

package mmap

import (
	"syscall"
	"unsafe"
)

const (
	PROT_READ  = 0x04
	PROT_WRITE = 0x02
)

const (
	MAP_SHARED = 1 << iota
	MAP_PRIVATE
)

var kernel32 = syscall.NewLazyDLL("Kernel32.dll")
var _MapViewOfFile = kernel32.NewProc("MapViewOfFile")
var _UnmapViewOfFile = kernel32.NewProc("UnmapViewOfFile")

func nsMapViewOfFile(hFile syscall.Handle, dwDesiredAccess uint32, FileOffset uint64, dwNumberOfBytesToMap uint64) (uintptr, error) {
	ret, _, err := _MapViewOfFile.Call(
		uintptr(hFile),
		uintptr(dwDesiredAccess),
		uintptr(FileOffset>>32),
		uintptr(FileOffset&0xffffffff),
		uintptr(dwNumberOfBytesToMap),
	)
	if ret == 0 {
		return 0, err
	}
	return ret, nil
}

func nsUnmapViewOfFile(lpBaseAddress uintptr) error {
	_, _, err := _UnmapViewOfFile.Call(lpBaseAddress)
	return err
}

func Map(fd uintptr, offset int, len int, prot int, flags int) ([]byte, error) {
	_ = flags

	if prot&(PROT_READ|PROT_WRITE) != 0 {
		prot = PROT_WRITE
	}

	base, err := nsMapViewOfFile(syscall.Handle(fd), uint32(prot), uint64(offset), uint64(len))
	if err != nil {
		return nil, err
	}
	return unsafe.Slice((*byte)(unsafe.Pointer(base)), len), nil
}

func UnMap(b []byte) error {
	return nsUnmapViewOfFile(uintptr(unsafe.Pointer(unsafe.SliceData(b))))
}
