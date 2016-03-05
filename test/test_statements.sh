#!/bin/bash

echo "TESTING STATEMENTS - SHOULD PASS..."
cat ../test/if_else.jt | tee ../src/interpreter

echo "TESTING STATEMENTS - SHOULD FAIL..."