#!/bin/bash

echo "TESTING COMMENTS - SHOULD PASS..."
echo "/* This is a comment */" | ../src/jateste.native
echo "/* */" | ../src/jateste.native
echo -e "/* Last one was empty \n This is two lines */" | ../src/jateste.native

echo "TESTING COMMENT - SHOULD FAIL..."
echo "Bad comment */" | ../src/jateste.native
echo "/* Another bad one" | ../src/jateste.native
