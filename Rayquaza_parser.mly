%{ open Ast %}

%token SEMI COLON PRINT LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF
/* Boolean operators */
%token NOT OR AND EQ NEQ
/* Loops and conditionals */
%token FOR WHILE IF ELIF ELSE
/* Mathematical operators */
%token PLUS MINUS TIMES DIVIDE MODULO
/*Function tokens */
%token DEF RETURN
%token <string> ID

%start expr
%type <Ast.expr> expr

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%
stmt:
| expr SEMI { $1 }
| LBRACE stmt_list RBRACE                 { Block $2 }
/* if (condition) { block1} else {block2} */
/* if (condition) stmt else stmt */
| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
| WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
/* return */
| RETURN expr SEMI                        { Return $2      }


expr:
| ID { Var $1 }
| LPAREN expr RPAREN { $2 }
| Print expr { Print $2 }
| EOF { Eof }
| expr PLUS   expr { Binop($1, Add,   $3)   }
| expr MINUS  expr { Binop($1, Sub,   $3)   }
| expr EQ     expr { Binop($1, Equal, $3)   }
| expr NEQ    expr { Binop($1, Neq, $3)     }
| expr LT     expr { Binop($1, Less,  $3)   }
| expr AND    expr { Binop($1, And,   $3)   }
| expr OR     expr { Binop($1, Or,    $3)   }
| ID ASSIGN expr   { Assign($1, $3)         }
| LPAREN expr RPAREN { $2                   }
/* call */
| ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
