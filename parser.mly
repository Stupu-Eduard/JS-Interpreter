%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> ID
%token LET IF ELSE WHILE TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LE GT GE AND OR NOT ASSIGN
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EOF

/* PRIORITATI: De la mic la mare */
%right ASSIGN             /* Foarte important pentru x = y = 10 */
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc NOT UNARY_MINUS
%nonassoc IF
%nonassoc ELSE
%token RETURN

%start <Ast.stmt> prog
%%

prog:
  | ss = list(stmt); EOF { Block ss }
  ;

expr:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | b = TRUE { Bool true }
  | b = FALSE { Bool false }
  | s = STRING { String s }
  | c = CHAR { Char c }
  | x = ID { Var x }
  /* ATRIBUIREA ESTE ACUM EXPRESIE */
  | x = ID; ASSIGN; e = expr { Assign(x, e) } 
  | e1 = expr; PLUS; e2 = expr { BinOp(e1, Add, e2) }
  | e1 = expr; MINUS; e2 = expr { BinOp(e1, Sub, e2) }
  | e1 = expr; TIMES; e2 = expr { BinOp(e1, Mul, e2) }
  | e1 = expr; DIVIDE; e2 = expr { BinOp(e1, Div, e2) }
  | e1 = expr; MOD; e2 = expr { BinOp(e1, Mod, e2) }
  | e1 = expr; EQ; e2 = expr { BinOp(e1, Eq, e2) }
  | e1 = expr; NEQ; e2 = expr { BinOp(e1, Neq, e2) }
  | e1 = expr; LT; e2 = expr { BinOp(e1, Lt, e2) }
  | e1 = expr; GT; e2 = expr { BinOp(e1, Gt, e2) }
  | e1 = expr; LE; e2 = expr { BinOp(e1, Le, e2) }
  | e1 = expr; GE; e2 = expr { BinOp(e1, Ge, e2) }
  | e1 = expr; AND; e2 = expr { BinOp(e1, And, e2) }
  | e1 = expr; OR; e2 = expr { BinOp(e1, Or, e2) }
  | MINUS; e = expr %prec UNARY_MINUS { UnOp(Neg, e) }
  | NOT; e = expr { UnOp(Not, e) }
  | LPAREN; e = expr; RPAREN { e }
  ;

stmt:
  | RETURN; e = expr; SEMICOLON { Return e }
  | LET; x = ID; ASSIGN; e = expr; SEMICOLON { Declare(x, Some e) }
  | LET; x = ID; SEMICOLON { Declare(x, None) }
  | e = expr; SEMICOLON { Expr e } /* Aici intra si x = 10; */
  | IF; LPAREN; e = expr; RPAREN; s1 = stmt; ELSE; s2 = stmt { If(e, s1, s2) }
  | IF; LPAREN; e = expr; RPAREN; s1 = stmt %prec IF { If(e, s1, Skip) }
  | WHILE; LPAREN; e = expr; RPAREN; s = stmt { While(e, s) }
  | LBRACE; ss = list(stmt); RBRACE { Block ss }
  | SEMICOLON { Skip }
  ;