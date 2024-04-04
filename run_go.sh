#!/bin/sh
echo "basic example"
cd go
go build -ldflags="-s -w" -gcflags=-B -o ./../main ./cmd/basic
cd ..
GODEBUG=asyncpreemptoff=1 ./main

echo "lemon-mint1 example"
cd go
go build -ldflags="-s -w" -gcflags=-B -o ./../main ./cmd/lemon-mint1 
cd ..
GODEBUG=asyncpreemptoff=1 ./main
