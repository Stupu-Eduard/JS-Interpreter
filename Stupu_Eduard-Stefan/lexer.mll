{
  open Parser
  exception SyntaxError of string
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let whitespace = [' ' '\t' '\r' '\n']+

rule read = parse
  | whitespace { read lexbuf }
  | "//" [^ '\n']* { read lexbuf } (* REGULA NOUA: Ignora comentariile pe o singura linie *)
  | "function" { FUNCTION }
  | "let"   { LET }
  | "return" { RETURN }
  | "if"    { IF }
  | "else"  { ELSE }
  | "while" { WHILE }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "=="    { EQ }
  | "!="    { NEQ }
  | "<="    { LE }
  | ">="    { GE }
  | "||"    { OR }
  | "&&"    { AND }
  | '='     { ASSIGN }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIVIDE }
  | '%'     { MOD }
  | '<'     { LT }
  | '>'     { GT }
  | '!'     { NOT }
  | ','     { COMMA }
  | ';'     { SEMICOLON }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | digit+ '.' digit* as v { FLOAT (float_of_string v) }
  | digit+ as v            { INT (int_of_string v) }
  | letter (letter | digit)* as n { ID n }
  | '\'' (_ as c) '\''     { CHAR c }
  | '"' ([^ '"']* as s) '"' { STRING s }
  | eof     { EOF }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }