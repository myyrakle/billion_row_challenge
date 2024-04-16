cd go && \
  go build -gccgoflags="-O3 -march=native -mtune=native -fgo-compiling-runtime -flto -finline-functions -fdevirtualize -fno-check-pointer-bounds -fno-go-check-divide-zero" -compiler gccgo -o ./../main ./cmd/lemon-mint3/main.go && \
  cd .. && \
  ./main
