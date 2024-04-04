#!/bin/sh
echo "BaseLine example"
cd go
go build -ldflags="-s -w" -gcflags=-B -o ./../main ./cmd/baseline 
cd ..
GODEBUG=asyncpreemptoff=1 ./main
