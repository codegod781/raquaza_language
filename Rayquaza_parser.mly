%{
open Rayquaza_ast
%}
%token SEMI COLON PRINT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA
%token EOF
/* Boolean operators */
%token NOT OR AND EQ NEQ LT
/* Loops and conditionals */
%token FOR WHILE IF ELSE
/* Mathematical operators */
%token PLUS MINUS TIMES DIVIDE MODULO
/* Function tokens */
%token DEF RETURN
%token <string> STRING
%token PRINT
%token <string> ID

%start program
%type <Rayquaza_ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program:
  | stmt_list { Program($1) }

stmt:
  | expr SEMI                          { Expr($1) }
  | LBRACE stmt_list RBRACE            { Block $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt      { While($3, $5) }
  | RETURN expr SEMI                   { Return $2 }

stmt_list:
  | stmt { [ $1 ] }
  | stmt stmt_list { $1 :: $2 }

expr:
  | ID                                 { Var $1 }
  | STRING                             { StringLiteral $1 }
  | PRINT LPAREN expr RPAREN          { Call("print", [$3]) }
  | expr PLUS expr                     { Binop($1, Add, $3) }
  | expr MINUS expr                    { Binop($1, Sub, $3) }
  | expr EQ expr                       { Binop($1, Equal, $3) }
  | expr NEQ expr                      { Binop($1, Neq, $3) }
  | expr LT expr                       { Binop($1, Less, $3) }
  | expr AND expr                      { Binop($1, And, $3) }
  | expr OR expr                       { Binop($1, Or, $3) }
  | ID ASSIGN expr                     { Assign($1, $3) }
  | LPAREN expr RPAREN                 { $2 }
  | ID LPAREN args_opt RPAREN          { Call($1, $3) }

args_opt:
  | /* nothing */                      { [] }
  | args                               { $1 }

args:
  | expr                               { [$1] }
  | expr COMMA args                    { $1 :: $3 }
