#!/bin/bash
swiftc -O -whole-module-optimization swift/basic/main.swift -o swift/basic/basic.run
swift/basic/basic.run
