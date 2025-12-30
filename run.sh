#!/bin/bash

# Oprește scriptul la prima eroare întâlnită
set -e

echo "--- 1. Curățare fișiere vechi ---"
rm -f *.cmi *.cmo parser.ml parser.mli lexer.ml interpretor

echo "--- 2. Setare mediu OPAM ---"
eval $(opam env)

echo "--- 3. Compilare AST ---"
ocamlc -c ast.ml

echo "--- 4. Generare Parser (Menhir) ---"
menhir --infer parser.mly

echo "--- 5. Generare Lexer (Ocamllex) ---"
ocamllex lexer.mll

echo "--- 6. Compilare Parser și Lexer ---"
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml

echo "--- 7. Compilare Evaluator ---"
ocamlc -c eval.ml

echo "--- 8. Compilare Main ---"
ocamlc -c main.ml

echo "--- 9. Linkare și generare Interpretor ---"
ocamlc -o interpretor ast.cmo parser.cmo lexer.cmo eval.cmo main.cmo

echo "--- Totul este gata!  Ruleaza ./interpretor ---"
