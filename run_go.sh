#!/bin/sh
echo "BASIC example"
cd go
go build -o ./../main cmd/basic/main.go 
cd ..
./main