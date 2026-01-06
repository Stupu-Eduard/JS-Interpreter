open Ast

(* === TIPURI PENTRU VALORI === *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VChar of char
  | VString of string
  | VUndefined

(* === ENVIRONMENT (starea programului) === *)
module Env = Map.Make(String)
type env = value Env.t

(* === REZULTATUL EXECUȚIEI === *)
type result =
  | Continue of env        (* execuția continuă normal *)
  | Exit of value * env    (* return a fost apelat *)

(* === EXCEPȚII PENTRU ERORI RUNTIME === *)
exception RuntimeError of string
exception UndefinedVariable of string
exception TypeError of string

(* === FUNCȚII AUXILIARE === *)

(* Convertește valoare la string pentru afișare *)
let string_of_value = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VChar c -> String.make 1 c
  | VString s -> "\"" ^ s ^ "\""
  | VUndefined -> "undefined"

(* Verifică dacă o valoare e "truthy" (pentru if/while) *)
let is_truthy = function
  | VBool false -> false
  | VInt 0 -> false
  | VFloat 0.0 -> false
  | VString "" -> false
  | VUndefined -> false
  | _ -> true

(* === EVALUAREA EXPRESIILOR === *)
(* Returnează (valoare, env_nou) deoarece Assign modifică starea *)

let rec eval_expr (env : env) (e : expr) : value * env =
  match e with
  (* Literali *)
  | Int i -> (VInt i, env)
  | Float f -> (VFloat f, env)
  | Bool b -> (VBool b, env)
  | Char c -> (VChar c, env)
  | String s -> (VString s, env)
  
  (* Variabile *)
  | Var x ->
      (try (Env.find x env, env)
       with Not_found -> raise (UndefinedVariable ("Variabila '" ^ x ^ "' nu este definită")))
  
  (* Atribuire (expresie!) *)
  | Assign (x, e1) ->
      let (v, env') = eval_expr env e1 in
      (* Verifică dacă variabila există *)
      if not (Env.mem x env') then
        raise (UndefinedVariable ("Variabila '" ^ x ^ "' nu este declarată"))
      else
        (v, Env.add x v env')  (* returnează valoarea ȘI actualizează env *)
  
  (* Operații binare *)
  | BinOp (e1, op, e2) ->
      let (v1, env') = eval_expr env e1 in
      let (v2, env'') = eval_expr env' e2 in  (* e2 se evaluează în env' *)
      (eval_binop op v1 v2, env'')
  
  (* Operații unare *)
  | UnOp (op, e1) ->
      let (v, env') = eval_expr env e1 in
      (eval_unop op v, env')

(* Evaluează operații binare *)
and eval_binop op v1 v2 =
  match (op, v1, v2) with
  (* Aritmetice pe Int *)
  | (Add, VInt a, VInt b) -> VInt (a + b)
  | (Sub, VInt a, VInt b) -> VInt (a - b)
  | (Mul, VInt a, VInt b) -> VInt (a * b)
  | (Div, VInt a, VInt b) -> 
      if b = 0 then raise (RuntimeError "Împărțire la zero")
      else VInt (a / b)
  | (Mod, VInt a, VInt b) -> 
      if b = 0 then raise (RuntimeError "Modulo la zero")
      else VInt (a mod b)
  
  (* Aritmetice pe Float *)
  | (Add, VFloat a, VFloat b) -> VFloat (a +. b)
  | (Sub, VFloat a, VFloat b) -> VFloat (a -. b)
  | (Mul, VFloat a, VFloat b) -> VFloat (a *. b)
  | (Div, VFloat a, VFloat b) -> VFloat (a /.  b)
  
  (* Aritmetice mixte Int/Float *)
  | (Add, VInt a, VFloat b) -> VFloat (float_of_int a +. b)
  | (Add, VFloat a, VInt b) -> VFloat (a +. float_of_int b)
  | (Sub, VInt a, VFloat b) -> VFloat (float_of_int a -.  b)
  | (Sub, VFloat a, VInt b) -> VFloat (a -. float_of_int b)
  | (Mul, VInt a, VFloat b) -> VFloat (float_of_int a *. b)
  | (Mul, VFloat a, VInt b) -> VFloat (a *. float_of_int b)
  | (Div, VInt a, VFloat b) -> VFloat (float_of_int a /.  b)
  | (Div, VFloat a, VInt b) -> VFloat (a /. float_of_int b)
  
  (* Concatenare String *)
  | (Add, VString a, VString b) -> VString (a ^ b)
  | (Add, VString a, VInt b) -> VString (a ^ string_of_int b)
  | (Add, VInt a, VString b) -> VString (string_of_int a ^ b)
  
  (* Comparații pe Int *)
  | (Lt, VInt a, VInt b) -> VBool (a < b)
  | (Le, VInt a, VInt b) -> VBool (a <= b)
  | (Gt, VInt a, VInt b) -> VBool (a > b)
  | (Ge, VInt a, VInt b) -> VBool (a >= b)
  | (Eq, VInt a, VInt b) -> VBool (a = b)
  | (Neq, VInt a, VInt b) -> VBool (a <> b)
  
  (* Comparații pe Float *)
  | (Lt, VFloat a, VFloat b) -> VBool (a < b)
  | (Le, VFloat a, VFloat b) -> VBool (a <= b)
  | (Gt, VFloat a, VFloat b) -> VBool (a > b)
  | (Ge, VFloat a, VFloat b) -> VBool (a >= b)
  | (Eq, VFloat a, VFloat b) -> VBool (a = b)
  | (Neq, VFloat a, VFloat b) -> VBool (a <> b)
  
  (* Comparații pe Bool *)
  | (Eq, VBool a, VBool b) -> VBool (a = b)
  | (Neq, VBool a, VBool b) -> VBool (a <> b)
  
  (* Comparații pe String *)
  | (Eq, VString a, VString b) -> VBool (a = b)
  | (Neq, VString a, VString b) -> VBool (a <> b)
  
  (* Operații logice *)
  | (And, VBool a, VBool b) -> VBool (a && b)
  | (Or, VBool a, VBool b) -> VBool (a || b)
  
  (* Eroare de tip *)
  | _ -> raise (TypeError ("Operație invalidă între tipuri incompatibile"))

(* Evaluează operații unare *)
and eval_unop op v =
  match (op, v) with
  | (Neg, VInt i) -> VInt (-i)
  | (Neg, VFloat f) -> VFloat (-.f)
  | (Not, VBool b) -> VBool (not b)
  | (Not, v) -> VBool (not (is_truthy v))  (* !  pe non-bool folosește truthy *)
  | _ -> raise (TypeError "Operație unară invalidă")

(* === EXECUȚIA INSTRUCȚIUNILOR === *)
(* Returnează result:  Continue env | Exit (value, env) *)

let rec exec_stmt (env :  env) (s : stmt) : result =
  match s with
  (* Skip - nu face nimic *)
  | Skip -> Continue env
  
  (* Declarație cu/fără inițializare *)
  | Declare (x, init_opt) ->
      let (v, env') = match init_opt with
        | Some e -> eval_expr env e
        | None -> (VUndefined, env)
      in
      Continue (Env.add x v env')
  
  (* Expresie ca statement (ex:  x = 5;) *)
  | Expr e ->
      let (_, env') = eval_expr env e in
      Continue env'
  
  (* Bloc de instrucțiuni *)
  | Block stmts ->
      exec_block env stmts
  
  (* If-Else *)
  | If (cond, s1, s2) ->
      let (v, env') = eval_expr env cond in
      if is_truthy v then
        exec_stmt env' s1
      else
        exec_stmt env' s2
  
  (* While *)
  | While (cond, body) ->
      exec_while env cond body
  
  (* Return *)
  | Return e ->
      let (v, env') = eval_expr env e in
      Exit (v, env')

(* Execută un bloc de instrucțiuni cu Scoping *)
and exec_block env stmts =
  let rec loop current_env = function
    | [] -> 
        let env_dupa_bloc = Env.fold (fun cheie valoare acc ->
          if Env.mem cheie env then 
            Env.add cheie valoare acc 
          else 
            acc
        ) current_env env in
        Continue env_dupa_bloc
        
    | s :: rest ->
        (match exec_stmt current_env s with
         | Continue env' -> loop env' rest
         | Exit (v, env') -> Exit (v, env'))
  in loop env stmts

and exec_while env cond body =
  let (v, env') = eval_expr env cond in
  if not (is_truthy v) then
    Continue env'
  else
    match exec_stmt env' body with
    | Continue env'' -> exec_while env'' cond body 
    | Exit (v, env'') -> Exit (v, env'')  

(* === FUNCȚIA PRINCIPALĂ === *)

let run (program : stmt) : unit =
  let empty_env = Env.empty in
  try
    match exec_stmt empty_env program with
    | Continue final_env ->
        print_endline "\n✅ Program executat cu succes. ";
        print_endline "Starea finală a variabilelor:";
        Env.iter (fun k v -> 
          Printf.printf "  %s = %s\n" k (string_of_value v)
        ) final_env
    | Exit (v, _) ->
        Printf.printf "\n✅ Program terminat cu return:  %s\n" (string_of_value v)
  with
  | RuntimeError msg -> Printf.printf "❌ RuntimeError: %s\n" msg
  | UndefinedVariable msg -> Printf.printf "❌ UndefinedVariable: %s\n" msg
  | TypeError msg -> Printf.printf "❌ TypeError:  %s\n" msg