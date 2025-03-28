#!/bin/bash
EXEC_NAME='main'
COMPLILER='clang++'

# -s -fvisibility=hidden -flto
FLAGS='-std=c++23 -Wno-c++20-extensions -Wno-c++23-extensions'
OLVL='-O0'
GLVL='-g3'
LLVM_CONF='-lLLVM-19'
# LLVM_CONF=`llvm-config --cxxflags --ldflags --libs --system-libs` #no exceptions no unwinding tables --std=C++17

WORKING_DIR="$(pwd)"
LIBS_DIR="$(dirname "$WORKING_DIR")/libs"

# echo $LLVM_CONF
echo "Executable: $EXEC_NAME"
echo "$COMPLILER $OLVL $GLVL $FLAGS $LLVM_CONF"

front(){
  echo "Compiling the frontend"
    $COMPLILER $GLVL $OLVL $FLAGS --shared -o lexer.so ./frontend/lexer.cpp -fPIC &
    $COMPLILER $GLVL $OLVL $FLAGS --shared -o parser.so ./frontend/parser.cpp -fPIC &
    $COMPLILER $GLVL $OLVL $FLAGS --shared -o semantics.so ./frontend/semantics.cpp -fPIC &
  wait
}

front

echo "Compiling Main"
$COMPLILER $GLVL $OLVL $FLAGS $LLVM_CONF  -o $EXEC_NAME  ./main.cpp ./semantics.so ./parser.so ./lexer.so -lboost_program_options
