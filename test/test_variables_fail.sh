#!/bin/bash

echo "TESTING VARIABLES - SHOULD ALL FAIL..."
echo "int 9;" | tee ../src/interpreter
echo "double &Z_as;" | tee ../src/interpreter
echo "char #fsdsd;" | tee ../src/interpreter
echo "string )abcs(;" | tee ../src/interpreter

echo "int a; a = 'x';" | tee ../src/interpreter
echo "double b; b = \"hello\";" | tee ../src/interpreter
echo "char c; c = \"world\";" | tee ../src/interpreter
echo "string d; d = 10;" | tee ../src/interpreter