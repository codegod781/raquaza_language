%{
open Rayquaza_ast
%}

%token SEMI COLON PRINT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA ASSIGN
%token EOF
/* Python tokens */
%token IN RANGE APPEND POP 

/* Boolean operators */
%token NOT OR AND EQ NEQ LT GT
/* Loops and conditionals */
%token FOR WHILE IF ELSE ELIF THEN
/* Mathematical operators */
%token PLUS MINUS TIMES DIVIDE MODULO
/* Function tokens */
%token DEF RETURN
%token <string> STRING
%token <string> ID
%token <int> LITERAL
%token <bool> BLIT

%start program
%type <Rayquaza_ast.program> program

/* Precedence and associativity rules */
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc LBRACKET RBRACKET
%nonassoc NOELSE
%nonassoc ELSE

%%

program:
  stmt_list { Program($1) }

stmt_list:
  | stmt { [ $1 ] }
  | stmt stmt_list { $1 :: $2 }



stmt:
  expr SEMI { Expr($1) }
  | LBRACE stmt_list RBRACE { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | RETURN expr SEMI { Return($2) }
  | DEF ID LPAREN formals_list RPAREN LBRACE stmt_list RBRACE { Func($2, $4, $7) }

formals_list:
  ID { [$1] }
  | ID COMMA formals_list { $1::$3 }

expr:
  LITERAL { Literal($1) }
  | BLIT { BoolLit($1) }
  | ID { Var($1) }
  | STRING { StringLiteral($1) }
  | PRINT LPAREN expr RPAREN { Call("print", [$3]) }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr TIMES expr { Binop($1, Mul, $3) }
  | expr MODULO expr { Binop($1, Mod, $3) }
  | expr EQ expr { Binop($1, Equal, $3) }
  | expr NEQ expr { Binop($1, Neq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | LBRACKET expr_list RBRACKET { ArrayLit($2) }
  | expr LBRACKET expr RBRACKET { ArrayAccess($1, $3) }
  | expr LBRACKET expr RBRACKET ASSIGN expr { ArrayAssign($1, $3, $6) }
  | ID ASSIGN expr { Assign($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }

args_opt:
  /* nothing */ { [] }
  | args { $1 }

args:
  expr { [$1] }
  | expr COMMA args { $1 :: $3 }

expr_list:
  /* empty */ { [] }
  | expr_list_nonempty { $1 }

expr_list_nonempty:
  expr { [$1] }
  | expr COMMA expr_list_nonempty { $1 :: $3 }
