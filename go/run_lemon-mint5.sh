cd go
go build -gcflags=-B -o ./../main ./cmd/lemon-mint5
cd ..
GODEBUG=asyncpreemptoff=1 ./main
