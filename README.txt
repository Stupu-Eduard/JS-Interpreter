Proiect: Interpretor JavaScript-PLP
Autor: Stupu Eduard-Ștefan

---

# Cum compilezi și rulezi interpretorul

1. Asigură-te că ai OCaml, menhir și ocamllex instalate (pe Linux, recomandat cu OPAM).
2. În terminal, rulează:
   ./run.sh
   (Acest script va compila totul și va genera executabilul `interpretor`.)

3. Scrie programul tău în fișierul test.js (sau copiază un test din teste_de_incercat).
4. Rulează interpretorul cu:
   ./interpretor

# Teste
- Fișierul teste_de_incercat conține multe exemple de programe și rezultatele așteptate.
- Pentru a testa, copiază un program din teste_de_incercat în test.js și rulează interpretorul.

# Fișiere principale
- ast.ml, eval.ml, lexer.mll, parser.mly, main.ml: sursa interpretorului
- run.sh: script de build
- test.js: programul curent de rulat
- teste_de_incercat: colecție de teste

# Documentație
- Pentru detalii despre limbaj, vezi semantica_proiectare.pdf

Succes la utilizare!
