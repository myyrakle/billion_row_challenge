#!/bin/bash

# OCaml 컴파일 및 실행 (네이티브 코드)
ocamlopt -O2 -I +unix unix.cmxa -o ocaml/basic/basic.run ocaml/basic/main.ml && ocaml/basic/basic.run
