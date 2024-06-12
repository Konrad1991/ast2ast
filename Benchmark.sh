#!/bin/bash

g++ Benchmark.cpp -std=c++20 -I ./inst/include/ -pg
./a.out
gprof ./a.out gmon.out >analysis.txt
head analysis.txt -n 15
time ./a.out
