#!/bin/bash

echo "TESTING COMMENTS - SHOULD PASS..."
echo "/* This is a comment */" | tee ../src/interpreter
echo "/* */" | tee ../src/interpreter
echo -e "/* Last one was empty \n This is two lines */" | tee ../src/interpreter

echo "TESTING COMMENT - SHOULD FAIL..."
echo "Bad comment */" | tee ../src/interpreter
echo "/* Another bad one" | tee ../src/interpreter