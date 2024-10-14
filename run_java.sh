#!/bin/bash
echo "[Java] basic example (with g1gc)"
sh java/run_basic_with_g1gc.sh
echo "[Java] basic example (with zgc)"
sh java/run_basic_with_zgc.sh
echo "[Java] basic example (with parallelgc)"
sh java/run_basic_parellel.sh