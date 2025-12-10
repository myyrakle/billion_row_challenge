#!/bin/sh
# Kotlin Native로 컴파일 및 실행
kotlinc-native ./kotlin/basic/MainNative.kt -o ./kotlin/basic/main
./kotlin/basic/main.kexe
