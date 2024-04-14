cd go
go build -gcflags=-B -o ./../main ./cmd/lemon-mint1 
cd ..
GODEBUG=asyncpreemptoff=1 ./main
