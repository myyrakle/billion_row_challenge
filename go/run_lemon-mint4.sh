#!/bin/sh
set -eu

cd go/cmd/lemon-mint4
target="$(go env GOOS)/$(go env GOARCH)"
case "$target" in
	darwin/amd64 | linux/amd64)
		GOEXPERIMENT=simd GOAMD64=v3 \
			go build -gcflags=-B -trimpath -o ../../../main .
		;;
	darwin/arm64 | linux/arm64)
		GOEXPERIMENT= \
			go build -gcflags=-B -trimpath -o ../../../main .
		;;
	*)
		GOEXPERIMENT= \
			go build -gcflags=-B -trimpath -o ../../../main .
		;;
esac
cd ../../..
GODEBUG=asyncpreemptoff=1 ./main
