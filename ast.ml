type uoperator = Neg | Not
type operator = 
    | Add | Sub | Mul | Div | Mod
    | Gt  | Ge  | Lt  | Le
    | Eq  | Neq
    | And | Or

type expr =
    | Int of int
    | Float of float
    | Bool of bool
    | Char of char
    | String of string
    | Var of string
    | Assign of string * expr  (* Expresie acum! *)
    | BinOp of expr * operator * expr
    | UnOp of uoperator * expr
    | Call of string * expr list          
    | FuncExpr of string list * stmt   

and stmt =
    | Skip
    | Declare of string * expr option
    | Expr of expr
    | Block of stmt list
    | If of expr * stmt * stmt
    | While of expr * stmt
    | Return of expr 
    | FuncDecl of string * string list * stmt
    
(* --- Functii de afisare (pentru Debug) --- *)

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Char c -> "'" ^ String.make 1 c ^ "'"
  | Var x -> x
  | Assign (x, e) -> "(" ^ x ^ " = " ^ (string_of_expr e) ^ ")" (* Mutat aici! *)
  | Call (f, args) ->
      let rendered_args = String.concat ", " (List.map string_of_expr args) in
      f ^ "(" ^ rendered_args ^ ")"
  | FuncExpr (params, _) ->
      let rendered_params = String.concat ", " params in
      "function(" ^ rendered_params ^ ") { ... }"
  | BinOp (e1, op, e2) ->
      let op_s = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
        | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
        | And -> "&&" | Or -> "||"
      in "(" ^ string_of_expr e1 ^ " " ^ op_s ^ " " ^ string_of_expr e2 ^ ")"
  | UnOp (Neg, e) -> "-(" ^ string_of_expr e ^ ")"
  | UnOp (Not, e) -> "!(" ^ string_of_expr e ^ ")"

let rec print_ast indent = function
  | Block ss -> 
      Printf.printf "%s{\n" indent;
      List.iter (print_ast (indent ^ "  ")) ss;
      Printf.printf "%s}\n" indent
  | Declare (x, None) -> Printf.printf "%sLET %s;\n" indent x
  | Declare (x, Some e) -> Printf.printf "%sLET %s = %s;\n" indent x (string_of_expr e)
  | If (c, s1, s2) -> 
      Printf.printf "%sIF (%s)\n" indent (string_of_expr c);
      print_ast (indent ^ "  ") s1;
      Printf.printf "%sELSE\n" indent;
      print_ast (indent ^ "  ") s2
  | While (c, s) ->
      Printf.printf "%sWHILE (%s)\n" indent (string_of_expr c);
      print_ast (indent ^ "  ") s
  | Expr e -> Printf.printf "%s%s;\n" indent (string_of_expr e) (* Acum Assign va fi printat prin Expr *)
  | Skip -> Printf.printf "%sSKIP;\n" indent
  | Return e -> Printf.printf "%sRETURN %s;\n" indent (string_of_expr e)
  | FuncDecl (name, params, body) ->
      let rendered_params = String.concat ", " params in
      Printf.printf "%sFUNCTION %s(%s)\n" indent name rendered_params;
      print_ast (indent ^ "  ") body