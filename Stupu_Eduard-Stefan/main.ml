open Ast
open Eval

let () =
  (* Eliminat spațiul din numele fișierului *)
  let filename = "test.js" in 
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    close_in chan;
    
    print_endline "=== AST ===";
    Ast.print_ast "" ast;
    print_endline "[OK] Parserul a construit arborele.";
    
    print_endline "\n=== EXECUȚIE ===";
    Eval.run ast
    
  with
  | Lexer.SyntaxError msg ->
      Printf.printf "[EROARE] Eroare Lexicala: %s\n" msg;
      close_in chan
  | Parser.Error ->
      (let pos = lexbuf.Lexing.lex_curr_p in
      Printf.printf "[EROARE] Eroare de Sintaxa la linia %d, coloana %d (langa jetonul '%s')\n" 
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Lexing.lexeme lexbuf);
      close_in chan)
  | Eval.RuntimeError msg ->
      Printf.printf "[EROARE] RuntimeError: %s\n" msg;
      close_in chan
  | Eval.UndefinedVariable msg ->
      Printf.printf "[EROARE] UndefinedVariable: %s\n" msg;
      close_in chan
  | Eval.TypeError msg ->
      Printf.printf "[EROARE] TypeError: %s\n" msg;
      close_in chan
  | e ->
      Printf.printf "[EROARE] Eroare neasteptata: %s\n" (Printexc.to_string e);
      close_in chan