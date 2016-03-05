#!/bin/bash

echo "TESTING VARIABLES - SHOULD ALL PASS..."
echo "int a;" | tee ../src/interpreter
echo "double a;" | tee ../src/interpreter
echo "char a;" | tee ../src/interpreter
echo "string a;" | tee ../src/interpreter

echo "int a_var;" | tee ../src/interpreter
echo "double _a_var;" | tee ../src/interpreter
echo "char Var_a;" | tee ../src/interpreter
echo "string __VAR;" | tee ../src/interpreter

echo "int a; a = 10;" | tee ../src/interpreter
echo "double a; a = 2.5" | tee ../src/interpreter
echo "char a; a = 'h';" | tee ../src/interpreter
echo "string a; a = \"hello\"" | tee ../src/interpreter