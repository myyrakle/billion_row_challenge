//go:build unix || linux || darwin || freebsd || openbsd
// +build unix linux darwin freebsd openbsd

package mmap

import "syscall"

const (
	PROT_READ  = syscall.PROT_READ
	PROT_WRITE = syscall.PROT_WRITE

	MAP_SHARED  = syscall.MAP_SHARED
	MAP_PRIVATE = syscall.MAP_PRIVATE
)

func Map(fd uintptr, offset int, len int, prot int, flags int) ([]byte, error) {
	return syscall.Mmap(int(fd), int64(offset), len, prot, flags)
}

func UnMap(b []byte) error {
	return syscall.Munmap(b)
}
