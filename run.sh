#!/bin/bash

<<COMMENT
    Compiles .sk file into LLVM IR code
    Then static compiles with llc
    Then links with utils.cpp to generate .out executable file
    Run executable like ./{base}.out
COMMENT

filename=$1
base=${filename%.*}
rm -f ${base}.ir ${base}.ir.s ${base}.out
./skan.native -l ${filename} >> ${base}.ir
/usr/local/opt/llvm/bin/llc -relocation-model=pic ${base}.ir
g++ -g -Wall `pkg-config --cflags --libs opencv` -std=c++11 ${base}.ir.s utils.cpp -o ${base}.out