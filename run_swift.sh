#!/bin/bash

# Swift 직접 컴파일 (Package Manager 없이)
swiftc -O -whole-module-optimization swift/basic/main.swift -o swift/basic/basic.run

# 실행
swift/basic/basic.run
