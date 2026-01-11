open Ast

(* === ENVIRONMENT (starea programului) === *)
module EnvMap = Map.Make(String)

(* === TIPURI PENTRU VALORI === *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VChar of char
  | VString of string
  | VUndefined
  | VFunc of string list * stmt * env

(* === ENVIRONMENT TYPE === *)
(* Păstrăm atât map-ul cât și ordinea declarării *)
and env = {
  values: value EnvMap.t;
  order: string list;  (* Lista cheilor în ordinea declarării *)
}

(* Helper functions pentru env *)
let env_empty = { values = EnvMap.empty; order = [] }

let env_add key value env =
  if EnvMap.mem key env.values then
    (* Variabila există deja, doar actualizăm valoarea *)
    { env with values = EnvMap.add key value env.values }
  else
    (* Variabilă nouă, adăugăm și în order *)
    { values = EnvMap.add key value env.values; order = env.order @ [key] }

let env_find key env = EnvMap.find key env.values
let env_find_opt key env = EnvMap.find_opt key env.values
let env_mem key env = EnvMap.mem key env.values

(* Iterează în ordinea declarării *)
let env_iter_ordered f env =
  List.iter (fun key ->
    match EnvMap.find_opt key env.values with
    | Some v -> f key v
    | None -> ()
  ) env.order

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
  | VFunc _ -> "<function>"

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
      (try (env_find x env, env)
       with Not_found -> raise (UndefinedVariable ("Variabila '" ^ x ^ "' nu este definită")))
  
  (* Atribuire (expresie!) *)
  | Assign (x, e1) ->
      let (v, env') = eval_expr env e1 in
      (* Verifică dacă variabila există *)
      if not (env_mem x env') then
        raise (UndefinedVariable ("Variabila '" ^ x ^ "' nu este declarată"))
      else
        (v, env_add x v env')  (* returnează valoarea ȘI actualizează env *)

  (* Apel funcție *)
  | Call (fname, args) ->
      let (evaluated_args, env_after_args) = eval_args env args in
      let func_val =
        try env_find fname env_after_args
        with Not_found -> raise (UndefinedVariable ("Funcția '" ^ fname ^ "' nu este definită"))
      in
      (match func_val with
       | VFunc (params, body, closure_env) ->
           if List.length params <> List.length evaluated_args then
             raise (RuntimeError "Număr de argumente incorect la apelul funcției")
           else
             (* Adaugam functia in call_env pentru a suporta recursivitatea *)
             let call_env_with_func = env_add fname func_val closure_env in
             let call_env =
               List.fold_left2 (fun acc param arg -> env_add param arg acc) call_env_with_func params evaluated_args
             in
             (match exec_stmt call_env body with
              | Exit (ret, _) -> (ret, env_after_args)
              | Continue _ -> (VUndefined, env_after_args))
       | _ -> raise (TypeError ("Valoarea '" ^ fname ^ "' nu este apelabilă")))

  (* Expresie funcție (funcție anonimă) *)
  | FuncExpr (params, body) -> (VFunc (params, body, env), env)
  
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

and eval_args env args =
  let rec aux acc current_env = function
    | [] -> (List.rev acc, current_env)
    | a :: rest ->
        let (v, env') = eval_expr current_env a in
        aux (v :: acc) env' rest
  in
  aux [] env args

(* === EXECUȚIA INSTRUCȚIUNILOR === *)
(* Returnează result:  Continue env | Exit (value, env) *)

and exec_stmt (env :  env) (s : stmt) : result =
  match s with
  (* Skip - nu face nimic *)
  | Skip -> Continue env
  
  (* Declarație cu/fără inițializare *)
  | Declare (x, init_opt) ->
      let (v, env') = match init_opt with
        | Some e -> eval_expr env e
        | None -> (VUndefined, env)
      in
      Continue (env_add x v env')
  
  (* Expresie ca statement (ex:  x = 5;) *)
  | Expr e ->
      let (_, env') = eval_expr env e in
      Continue env'
  
  (* Bloc de instrucțiuni *)
  | Block stmts ->
      exec_block env stmts

    (* Declarație funcție *)
    | FuncDecl (name, params, body) ->
      let func_val = VFunc (params, body, env) in
      Continue (env_add name func_val env)
  
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
(* NOTĂ: Pentru blocuri { } explicite, variabilele locale sunt eliminate la final *)
(* Pentru blocul top-level (programul principal), vrem să păstrăm toate variabilele *)
and exec_block ?(is_toplevel=false) env stmts =
  (* Ținem evidența variabilelor DECLARATE (nu doar modificate) în acest bloc *)
  let rec loop current_env declared_in_block = function
    | [] -> 
        if is_toplevel then
          (* Pentru top-level, păstrăm toate variabilele *)
          Continue current_env
        else
          (* Pentru blocuri locale:
             - Eliminăm variabilele declarate în bloc
             - Restaurăm valorile originale pentru variabilele care au fost shadowed
             - Păstrăm modificările pentru variabilele exterioare care au fost doar atribuite *)
          let env_dupa_bloc = List.fold_left (fun acc cheie ->
            let valoare_originala = env_find cheie env in
            if List.mem cheie declared_in_block then
              (* Variabila a fost declarată în bloc - folosim valoarea originală *)
              env_add cheie valoare_originala acc
            else
              (* Variabila nu a fost re-declarată - folosim valoarea curentă *)
              match env_find_opt cheie current_env with
              | Some v -> env_add cheie v acc
              | None -> env_add cheie valoare_originala acc
          ) env_empty env.order in
          Continue env_dupa_bloc
        
    | s :: rest ->
        (match s with
         | Declare (name, _) when env_mem name env ->
             (* Variabila exista deja - este shadowing, o adăugăm la lista *)
             (match exec_stmt current_env s with
              | Continue env' -> loop env' (name :: declared_in_block) rest
              | Exit (v, env') -> Exit (v, env'))
         | _ ->
             (match exec_stmt current_env s with
              | Continue env' -> loop env' declared_in_block rest
              | Exit (v, env') -> Exit (v, env')))
  in loop env [] stmts

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
  let empty_env = env_empty in
  try
    (* Pentru programul principal (Block top-level), folosim is_toplevel=true *)
    let result = match program with
      | Block stmts -> exec_block ~is_toplevel:true empty_env stmts
      | _ -> exec_stmt empty_env program
    in
    match result with
    | Continue final_env ->
        print_endline "\n[OK] Program executat cu succes.";
        print_endline "Starea finală a variabilelor:";
        env_iter_ordered (fun k v -> 
          Printf.printf "  %s = %s\n" k (string_of_value v)
        ) final_env
    | Exit (v, _) ->
        Printf.printf "\n[OK] Program terminat cu return: %s\n" (string_of_value v)
  with
  | RuntimeError msg -> Printf.printf "[EROARE] RuntimeError: %s\n" msg
  | UndefinedVariable msg -> Printf.printf "[EROARE] UndefinedVariable: %s\n" msg
  | TypeError msg -> Printf.printf "[EROARE] TypeError: %s\n" msg