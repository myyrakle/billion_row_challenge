#!/bin/bash
dmd -O -release -inline d/basic/main.d -od=d/basic -of=d/basic/basic
./d/basic/basic
