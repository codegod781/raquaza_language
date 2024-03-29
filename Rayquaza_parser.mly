%{ open Ast %}

%token SEMI COLON PRINT LPAREN RPAREN LBRACKET RBRACKET
%token EOF
/* Boolean operators */
%token NOT OR AND
/* Loops and conditionals */
%token FOR WHILE IF ELIF ELSE
/* Mathematical operators */
%token PLUS MINUS TIMES DIVIDE MODULO
/*Function tokens */
%token DEF RETURN
%start expr
%type <Ast.expr> expr

%%
stmt:
| expr SEMI { $1 }


expr:
| ID { Var $1 }
| LPAREN expr RPAREN { $2 }
| Print expr { Print $2 }
| EOF { Eof }
