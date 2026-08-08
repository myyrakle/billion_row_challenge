//go:build linux

package main

import "syscall"

func adviseMapped(data []byte) {
	_ = syscall.Madvise(data, syscall.MADV_SEQUENTIAL)
	_ = syscall.Madvise(data, syscall.MADV_HUGEPAGE)
	_ = syscall.Madvise(data, syscall.MADV_WILLNEED)
}
