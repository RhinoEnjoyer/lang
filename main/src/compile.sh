#!/bin/bash

EXEC_NAME='main'
COMPLILER='clang++'
FLAGS='-std=c++23 -fno-exceptions  -Wno-c++20-extensions -Wno-c++23-extensions -flto'
OLVL='-Os' 
GLVL='-g0'
LLVM_CONF='-lLLVM-18'
# LLVM_CONF=`llvm-config --cxxflags --ldflags --libs --system-libs` #no exceptions no unwinding tables --std=C++17

WORKING_DIR="$(pwd)"
LIBS_DIR="$(dirname "$WORKING_DIR")/libs"

# echo $LLVM_CONF
echo "Executable: $EXEC_NAME"
echo "$COMPLILER $OLVL $GLVL $FLAGS $LLVM_CONF"

$COMPLILER $GLVL $OLVL $FLAGS --shared -o parser.so ./frontend/parser.cpp -fPIC & 
$COMPLILER $GLVL $OLVL $FLAGS --shared -o lexer.so ./frontend/lexer.cpp -fPIC & 

wait

echo "Compiling Main"
$COMPLILER $GLVL $OLVL $FLAGS $LLVM_CONF -o $EXEC_NAME ./main.cpp  ./parser.so ./lexer.so

