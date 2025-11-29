#!/bin/bash
fpc -O3 -XX -CX pascal/basic/main.pas -opascal/basic/basic.run 2>&1 | grep -v "Note:"
pascal/basic/basic.run
