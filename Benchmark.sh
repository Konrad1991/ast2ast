#!/bin/bash

g++ Benchmark.cpp -std=c++20 -I ./inst/include/ -pg
./a.out
gprof ./a.out gmon.out >analysis.txt
cat analysis.txt
