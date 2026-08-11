#!/bin/bash

# COBOL 컴파일 스크립트
# GnuCOBOL (cobc) 사용

set -e

echo "=== COBOL Billion Row Challenge Compiler ==="
echo ""

# 1. cobc 설치 확인
if ! command -v cobc &> /dev/null; then
    echo "❌ GnuCOBOL (cobc)이 설치되어 있지 않습니다."
    echo ""
    echo "설치 방법:"
    echo "  Ubuntu/Debian: sudo apt-get install gnucobol"
    echo "  Arch Linux: sudo pacman -S gnucobol"
    echo "  macOS: brew install gnucobol"
    echo "  또는 https://sourceforge.net/projects/gnucobol/files/ 에서 다운로드"
    echo ""
    exit 1
fi

echo "✓ GnuCOBOL 버전: $(cobc --version | head -1)"
echo ""

# 2. 컴파일
echo "컴파일 중..."
cobc -x -o main main.cob

if [ $? -eq 0 ]; then
    echo "✓ 컴파일 성공: main"
    echo ""
    echo "실행하려면:"
    echo "  ./main"
else
    echo "❌ 컴파일 실패"
    exit 1
fi
