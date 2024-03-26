%{ open Ast %}

%token SEMI PRINT LPAREN RPAREN 
%token EOF

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
