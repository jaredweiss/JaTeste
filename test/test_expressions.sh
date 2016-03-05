#!/bin/bash

echo "TESTING EXPRESSIONS - SHOULD PASS..."
echo "35 - 6;" | tee ../src/interpreter
echo "4 + 2;" | tee ../src/interpreter
echo "39 * 3;" | tee ../src/interpreter
echo "14 / 2;" | tee ../src/interpreter

echo "x = 6;" | tee ../src/interpreter
echo "y = 9.9;" | tee ../src/interpreter

echo "7 == 7;" | tee ../src/interpreter
echo "9 != 24;" | tee ../src/interpreter
echo "2 < 9;" | tee ../src/interpreter
echo "7 <= 7;" | tee ../src/interpreter
echo "42 > 24;" | tee ../src/interpreter
echo "44 >= 9;" | tee ../src/interpreter

echo "! 0;" | tee ../src/interpreter
echo "1 && 1;" | tee ../src/interpreter
echo "1 || 0;" | tee ../src/interpreter

echo "TESTING EXPRESSIONS - SHOULD FAIL..."