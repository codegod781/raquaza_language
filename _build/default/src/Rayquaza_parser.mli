type token =
  | SEMI
  | COLON
  | PRINT
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | ASSIGN
  | EOF
  | IN
  | RANGE
  | APPEND
  | POP
  | NOT
  | OR
  | AND
  | EQ
  | NEQ
  | LT
  | GT
  | FOR
  | WHILE
  | IF
  | ELSE
  | ELIF
  | THEN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | DEF
  | RETURN
  | STRING of (
# 18 "src/Rayquaza_parser.mly"
        string
# 42 "src/Rayquaza_parser.mli"
)
  | ID of (
# 19 "src/Rayquaza_parser.mly"
        string
# 47 "src/Rayquaza_parser.mli"
)
  | LITERAL of (
# 20 "src/Rayquaza_parser.mly"
        int
# 52 "src/Rayquaza_parser.mli"
)
  | BLIT of (
# 21 "src/Rayquaza_parser.mly"
        bool
# 57 "src/Rayquaza_parser.mli"
)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Rayquaza_ast.program
