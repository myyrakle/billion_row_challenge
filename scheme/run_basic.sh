#!/bin/bash

# Scheme 실행 스크립트 (GNU Guile)
# 최적화된 바이트코드로 컴파일 후 실행

# -O3: 최고 수준 최적화
guild compile -O3 scheme/basic/main.scm -o scheme/basic/main.go 2>&1 | grep -v "note:"

# 원본 scm 파일 실행 (자동으로 컴파일된 .go 파일 사용)
# -C: 컴파일된 파일 경로 추가
guile -q -C scheme/basic scheme/basic/main.scm
