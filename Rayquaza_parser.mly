%{
open Rayquaza_ast
%}
%token SEMI COLON PRINT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA ASSIGN
%token EOF
/* python tokens*/
%token IN RANGE APPEND POP 

/* Boolean operators */
%token NOT OR AND EQ NEQ LT
/* Loops and conditionals */
%token FOR WHILE IF ELSE ELIF THEN
/* Mathematical operators */
%token PLUS MINUS TIMES DIVIDE MODULO
/* Function tokens */
%token DEF RETURN
%token <string> STRING
%token PRINT
%token <string> ID
%token <int> LITERAL
%token <bool> BLIT

%start program
%type <Rayquaza_ast.program> program



%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program:
  | stmt_list { Program($1) }

// decls:
//   | /* nothing */ { ([]) }
//   | vassign SEMI decls { ($1 :: $3) }
//   | fdecl decls { ( $1 :: $2 )}

// vassign:
//   | ID ASSIGN expr {($1, $3)} 

formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  ID { [$1] }
  | ID COMMA formals_list { $1::$3 }

// if_stmt:
//   | IF expr stmt { If ($2, $3, [], None) }
//   | IF expr stmt elif_blocks ELSE stmt { If ($2, $3, $4, Some $6) }
//   | IF expr stmt elif_blocks { If ($2, $3, $4, None) }

// elif_blocks:
//   | ELIF expr stmt { [($2, $3)] }
//   | elif_blocks ELIF expr stmt { ($3, $4) :: $1 }

// if_stmt:
//   | IF expr stmt elif_blocks optional_else { If ($2, $3, $4, $5) }

// elif_blocks:
//   | /* empty */                          { [] }
//   | ELIF expr stmt elif_blocks           { ($2, $3) :: $4 }

// optional_else:
//   | /* empty */                          { None }
//   | ELSE stmt                            { Some $2 }

stmt:
  | expr SEMI                          { Expr($1) }
  | LBRACE stmt_list RBRACE            { Block $2 }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  // | if_stmt  { $1 }
  | WHILE LPAREN expr RPAREN stmt      { While($3, $5) }
  | RETURN expr SEMI                   { Return $2 }
  | DEF ID LPAREN formals_list RPAREN LBRACE stmt_list RBRACE { Func($2, $4, $7) }

// if_stmt:
//   | IF LPAREN expr RPAREN THEN stmt ELSE stmt { If($3, $6, Some $8) }
//   | IF LPAREN expr RPAREN THEN stmt           { If($3, $6, None) }

stmt_list:
  | stmt { [ $1 ] }
  | stmt stmt_list { $1 :: $2 }

expr:
    LITERAL                            { Literal($1)            }
  | BLIT                               { BoolLit($1)            }
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

