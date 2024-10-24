#!/bin/bash

# perf stat -e cycles,instructions,cache-misses,cache-references,l1d-loads,l1d-load-misses,branches,branch-misses  ./main
perf record -e cpu-cycles,instructions,system_time -F 99 -g --call-graph=dwarf ./main
