//go:build !windows && !unix && !linux && !darwin && !freebsd && !openbsd
// +build !windows,!unix,!linux,!darwin,!freebsd,!openbsd

package mmap

import "syscall"

const (
	PROT_READ = 1 << iota
	PROT_WRITE
)

const (
	MAP_SHARED = 1 << iota
	MAP_PRIVATE
)

func Map(fd uintptr, offset int, len int, prot int, flags int) ([]byte, error) {
	return nil, syscall.ENOSYS
}

func UnMap(b []byte) error {
	return syscall.ENOSYS
}
