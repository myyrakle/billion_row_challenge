cd go
go build -ldflags="-s -w" -gcflags=-B -o ./../main ./cmd/lemon-mint1 
cd ..
GODEBUG=asyncpreemptoff=1 ./main