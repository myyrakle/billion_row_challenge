FROM ubuntu:latest

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install gcc-12 gccgo-12 curl -y && \ 
    ln -s /usr/bin/gccgo-12 /usr/bin/gccgo

ADD ./go/scripts /scripts

RUN chmod +x /scripts/*.sh && \
    /scripts/install_go.sh
ENV PATH="${PATH}:/usr/local/go/bin"

ADD ./go /app/go
WORKDIR /app

RUN chmod +x /app/**/*.sh
RUN cd go && \
    go build -gccgoflags="-O3 -march=native -mtune=native -flto -fgo-compiling-runtime -finline-functions -fdevirtualize -fno-check-pointer-bounds -fno-go-check-divide-zero" -compiler gccgo -o ./../main ./cmd/lemon-mint3/main.go && \
    cd ..

ENTRYPOINT [ "/app/main" ]
