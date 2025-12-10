#!/bin/sh
# JVM으로 실행
kotlinc ./kotlin/basic/Main.kt -include-runtime -d ./kotlin/basic/Main.jar
java -jar ./kotlin/basic/Main.jar
