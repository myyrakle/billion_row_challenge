#!/bin/bash
julia -O3 --check-bounds=no --math-mode=fast julia/basic/main.jl
