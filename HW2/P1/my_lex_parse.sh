#!/bin/sh

ocamllex scanner.mll # create scanner.ml
ocamlyacc parser.mly # create parser.ml and parser.mli
ocamlc -c ast.mli # compile AST types
ocamlc -c parser.mli # compile parser types
ocamlc -c scanner.ml # compile the scanner
ocamlc -c parser.ml # compile the parser
ocamlc -c calc.ml # compile the interpreter
ocamlc -o calc parser.cmo scanner.cmo calc.cmo
