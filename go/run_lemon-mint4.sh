#!/bin/sh
set -eu

cd go/cmd/lemon-mint4
target="$(GOTOOLCHAIN=go1.27rc2 go env GOOS)/$(GOTOOLCHAIN=go1.27rc2 go env GOARCH)"
case "$target" in
	darwin/amd64 | linux/amd64)
		GOTOOLCHAIN=go1.27rc2 GOEXPERIMENT=simd GOAMD64=v3 \
			go build -trimpath -o ../../../main .
		;;
	darwin/arm64 | linux/arm64)
		GOTOOLCHAIN=go1.27rc2 GOEXPERIMENT= \
			go build -trimpath -o ../../../main .
		;;
	*)
		GOTOOLCHAIN=go1.27rc2 GOEXPERIMENT= \
			go build -trimpath -o ../../../main .
		;;
esac
cd ../../..
./main
