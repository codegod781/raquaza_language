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
# 42 "src/Rayquaza_parser.ml"
)
  | ID of (
# 19 "src/Rayquaza_parser.mly"
        string
# 47 "src/Rayquaza_parser.ml"
)
  | LITERAL of (
# 20 "src/Rayquaza_parser.mly"
        int
# 52 "src/Rayquaza_parser.ml"
)
  | BLIT of (
# 21 "src/Rayquaza_parser.mly"
        bool
# 57 "src/Rayquaza_parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "src/Rayquaza_parser.mly"
open Rayquaza_ast
# 64 "src/Rayquaza_parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* COLON *);
  259 (* PRINT *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* LBRACE *);
  265 (* RBRACE *);
  266 (* COMMA *);
  267 (* ASSIGN *);
    0 (* EOF *);
  268 (* IN *);
  269 (* RANGE *);
  270 (* APPEND *);
  271 (* POP *);
  272 (* NOT *);
  273 (* OR *);
  274 (* AND *);
  275 (* EQ *);
  276 (* NEQ *);
  277 (* LT *);
  278 (* GT *);
  279 (* FOR *);
  280 (* WHILE *);
  281 (* IF *);
  282 (* ELSE *);
  283 (* ELIF *);
  284 (* THEN *);
  285 (* PLUS *);
  286 (* MINUS *);
  287 (* TIMES *);
  288 (* DIVIDE *);
  289 (* MODULO *);
  290 (* DEF *);
  291 (* RETURN *);
    0|]

let yytransl_block = [|
  292 (* STRING *);
  293 (* ID *);
  294 (* LITERAL *);
  295 (* BLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\005\000\005\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\007\000\007\000\008\000\008\000\006\000\006\000\
\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\002\000\003\000\005\000\007\000\005\000\
\003\000\008\000\001\000\003\000\001\000\001\000\001\000\001\000\
\004\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\006\000\003\000\
\003\000\004\000\000\000\001\000\001\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\013\000\014\000\043\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\000\000\029\000\
\005\000\000\000\000\000\000\000\009\000\000\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\042\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\008\000\000\000\
\000\000\000\000\038\000\000\000\000\000\012\000\000\000\000\000\
\007\000\000\000\010\000"

let yydgoto = "\002\000\
\015\000\016\000\017\000\018\000\075\000\022\000\055\000\056\000\
\023\000"

let yysindex = "\005\000\
\052\255\000\000\006\255\011\255\011\255\052\255\009\255\012\255\
\238\254\011\255\000\000\001\255\000\000\000\000\000\000\000\000\
\052\255\012\000\011\255\084\000\157\000\014\255\000\000\013\255\
\011\255\011\255\019\255\045\000\011\255\011\255\000\000\000\000\
\011\255\011\255\011\255\011\255\011\255\011\255\011\255\011\255\
\011\255\011\255\011\255\011\255\104\000\000\000\011\255\000\000\
\000\000\122\000\140\000\246\254\000\000\174\000\025\255\000\000\
\208\000\191\000\213\000\230\000\073\255\073\255\038\255\038\255\
\020\255\020\255\039\255\039\255\039\255\000\000\000\000\052\255\
\052\255\021\255\049\255\011\255\000\000\048\255\000\000\036\255\
\246\254\057\255\000\000\011\255\052\255\000\000\052\255\208\000\
\000\000\055\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\065\255\000\000\000\000\000\000\
\000\000\000\000\000\000\091\255\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\071\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\077\255\000\000\000\000\
\158\255\000\000\007\000\056\255\051\001\071\001\007\001\029\001\
\051\000\078\000\157\255\190\255\223\255\000\000\000\000\000\000\
\000\000\079\255\000\000\000\000\000\000\124\255\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\224\255\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\191\255\255\255\004\000\000\000\000\000\017\000\
\052\000"

let yytablesize = 603
let yytable = "\024\000\
\006\000\002\000\020\000\021\000\029\000\001\000\079\000\080\000\
\028\000\019\000\031\000\030\000\025\000\003\000\004\000\026\000\
\005\000\045\000\027\000\089\000\048\000\049\000\052\000\050\000\
\051\000\033\000\074\000\054\000\057\000\077\000\081\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\033\000\033\000\021\000\011\000\012\000\
\013\000\014\000\042\000\043\000\044\000\082\000\003\000\004\000\
\027\000\005\000\084\000\006\000\027\000\085\000\027\000\091\000\
\087\000\027\000\040\000\041\000\042\000\043\000\044\000\039\000\
\027\000\027\000\054\000\007\000\008\000\041\000\033\000\035\000\
\090\000\037\000\088\000\011\000\086\000\009\000\010\000\011\000\
\012\000\013\000\014\000\015\000\083\000\038\000\039\000\015\000\
\015\000\015\000\071\000\000\000\015\000\040\000\041\000\042\000\
\043\000\044\000\000\000\015\000\015\000\015\000\015\000\015\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\015\000\015\000\015\000\015\000\030\000\000\000\000\000\000\000\
\030\000\030\000\030\000\000\000\000\000\030\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\030\000\030\000\030\000\
\030\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\030\000\030\000\030\000\030\000\021\000\032\000\000\000\
\000\000\021\000\032\000\021\000\032\000\000\000\021\000\032\000\
\000\000\000\000\000\000\000\000\000\000\021\000\021\000\021\000\
\021\000\021\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\021\000\021\000\021\000\021\000\020\000\000\000\
\000\000\000\000\020\000\000\000\020\000\000\000\000\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\020\000\
\020\000\020\000\020\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\020\000\020\000\020\000\020\000\022\000\
\031\000\000\000\000\000\022\000\031\000\022\000\031\000\000\000\
\022\000\031\000\000\000\000\000\000\000\000\000\000\000\022\000\
\022\000\022\000\022\000\022\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\022\000\022\000\022\000\022\000\
\000\000\000\000\000\000\006\000\006\000\000\000\006\000\028\000\
\006\000\006\000\002\000\028\000\032\000\028\000\000\000\000\000\
\028\000\033\000\000\000\000\000\000\000\000\000\000\000\028\000\
\006\000\006\000\000\000\000\000\034\000\035\000\036\000\037\000\
\038\000\039\000\006\000\006\000\006\000\006\000\006\000\006\000\
\040\000\041\000\042\000\043\000\044\000\053\000\000\000\000\000\
\000\000\000\000\033\000\018\000\000\000\000\000\000\000\018\000\
\000\000\018\000\000\000\000\000\018\000\034\000\035\000\036\000\
\037\000\038\000\039\000\018\000\018\000\018\000\018\000\018\000\
\018\000\040\000\041\000\042\000\043\000\044\000\019\000\018\000\
\018\000\000\000\019\000\000\000\019\000\000\000\000\000\019\000\
\046\000\033\000\000\000\000\000\000\000\000\000\019\000\019\000\
\019\000\019\000\019\000\019\000\034\000\035\000\036\000\037\000\
\038\000\039\000\019\000\019\000\070\000\033\000\000\000\000\000\
\040\000\041\000\042\000\043\000\044\000\000\000\000\000\000\000\
\034\000\035\000\036\000\037\000\038\000\039\000\072\000\033\000\
\000\000\000\000\000\000\000\000\040\000\041\000\042\000\043\000\
\044\000\000\000\034\000\035\000\036\000\037\000\038\000\039\000\
\073\000\033\000\000\000\000\000\000\000\000\000\040\000\041\000\
\042\000\043\000\044\000\000\000\034\000\035\000\036\000\037\000\
\038\000\039\000\033\000\000\000\000\000\000\000\047\000\000\000\
\040\000\041\000\042\000\043\000\044\000\034\000\035\000\036\000\
\037\000\038\000\039\000\033\000\000\000\000\000\000\000\076\000\
\000\000\040\000\041\000\042\000\043\000\044\000\034\000\035\000\
\036\000\037\000\038\000\039\000\033\000\078\000\000\000\000\000\
\000\000\000\000\040\000\041\000\042\000\043\000\044\000\034\000\
\035\000\036\000\037\000\038\000\039\000\033\000\000\000\000\000\
\000\000\000\000\033\000\040\000\041\000\042\000\043\000\044\000\
\034\000\035\000\036\000\037\000\038\000\039\000\035\000\036\000\
\037\000\038\000\039\000\033\000\040\000\041\000\042\000\043\000\
\044\000\040\000\041\000\042\000\043\000\044\000\000\000\000\000\
\036\000\037\000\038\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\041\000\042\000\043\000\044\000\025\000\
\000\000\000\000\000\000\025\000\000\000\025\000\000\000\000\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\025\000\025\000\025\000\025\000\025\000\026\000\000\000\000\000\
\000\000\026\000\000\000\026\000\000\000\000\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\026\000\026\000\
\026\000\026\000\026\000\023\000\000\000\000\000\000\000\023\000\
\000\000\023\000\000\000\000\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\023\000\023\000\023\000\024\000\
\000\000\000\000\000\000\024\000\000\000\024\000\000\000\000\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\024\000\024\000\024\000"

let yycheck = "\006\000\
\000\000\000\000\004\000\005\000\004\001\001\000\072\000\073\000\
\010\000\004\001\017\000\011\001\004\001\003\001\004\001\004\001\
\006\001\019\000\037\001\085\000\007\001\009\001\004\001\025\000\
\026\000\006\001\037\001\029\000\030\000\005\001\010\001\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\040\000\041\000\
\042\000\043\000\044\000\006\001\006\001\047\000\036\001\037\001\
\038\001\039\001\031\001\032\001\033\001\005\001\003\001\004\001\
\001\001\006\001\011\001\008\001\005\001\026\001\007\001\009\001\
\008\001\010\001\029\001\030\001\031\001\032\001\033\001\007\001\
\017\001\018\001\076\000\024\001\025\001\007\001\006\001\005\001\
\087\000\005\001\084\000\005\001\081\000\034\001\035\001\036\001\
\037\001\038\001\039\001\001\001\076\000\021\001\022\001\005\001\
\006\001\007\001\047\000\255\255\010\001\029\001\030\001\031\001\
\032\001\033\001\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\255\255\255\255\255\255\029\001\
\030\001\031\001\032\001\033\001\001\001\255\255\255\255\255\255\
\005\001\006\001\007\001\255\255\255\255\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\255\255\255\255\255\255\
\029\001\030\001\031\001\032\001\033\001\001\001\001\001\255\255\
\255\255\005\001\005\001\007\001\007\001\255\255\010\001\010\001\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\255\255\255\255\
\255\255\029\001\030\001\031\001\032\001\033\001\001\001\255\255\
\255\255\255\255\005\001\255\255\007\001\255\255\255\255\010\001\
\255\255\255\255\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\029\001\030\001\031\001\032\001\033\001\001\001\
\001\001\255\255\255\255\005\001\005\001\007\001\007\001\255\255\
\010\001\010\001\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\255\255\255\255\255\255\029\001\030\001\031\001\032\001\033\001\
\255\255\255\255\255\255\003\001\004\001\255\255\006\001\001\001\
\008\001\009\001\009\001\005\001\001\001\007\001\255\255\255\255\
\010\001\006\001\255\255\255\255\255\255\255\255\255\255\017\001\
\024\001\025\001\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\034\001\035\001\036\001\037\001\038\001\039\001\
\029\001\030\001\031\001\032\001\033\001\001\001\255\255\255\255\
\255\255\255\255\006\001\001\001\255\255\255\255\255\255\005\001\
\255\255\007\001\255\255\255\255\010\001\017\001\018\001\019\001\
\020\001\021\001\022\001\017\001\018\001\019\001\020\001\021\001\
\022\001\029\001\030\001\031\001\032\001\033\001\001\001\029\001\
\030\001\255\255\005\001\255\255\007\001\255\255\255\255\010\001\
\005\001\006\001\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\017\001\018\001\019\001\020\001\
\021\001\022\001\029\001\030\001\005\001\006\001\255\255\255\255\
\029\001\030\001\031\001\032\001\033\001\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\005\001\006\001\
\255\255\255\255\255\255\255\255\029\001\030\001\031\001\032\001\
\033\001\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\005\001\006\001\255\255\255\255\255\255\255\255\029\001\030\001\
\031\001\032\001\033\001\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\006\001\255\255\255\255\255\255\010\001\255\255\
\029\001\030\001\031\001\032\001\033\001\017\001\018\001\019\001\
\020\001\021\001\022\001\006\001\255\255\255\255\255\255\010\001\
\255\255\029\001\030\001\031\001\032\001\033\001\017\001\018\001\
\019\001\020\001\021\001\022\001\006\001\007\001\255\255\255\255\
\255\255\255\255\029\001\030\001\031\001\032\001\033\001\017\001\
\018\001\019\001\020\001\021\001\022\001\006\001\255\255\255\255\
\255\255\255\255\006\001\029\001\030\001\031\001\032\001\033\001\
\017\001\018\001\019\001\020\001\021\001\022\001\018\001\019\001\
\020\001\021\001\022\001\006\001\029\001\030\001\031\001\032\001\
\033\001\029\001\030\001\031\001\032\001\033\001\255\255\255\255\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\029\001\030\001\031\001\032\001\033\001\001\001\
\255\255\255\255\255\255\005\001\255\255\007\001\255\255\255\255\
\010\001\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\001\001\255\255\255\255\
\255\255\005\001\255\255\007\001\255\255\255\255\010\001\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\001\001\255\255\255\255\255\255\005\001\
\255\255\007\001\255\255\255\255\010\001\255\255\255\255\255\255\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\001\001\
\255\255\255\255\255\255\005\001\255\255\007\001\255\255\255\255\
\010\001\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001"

let yynames_const = "\
  SEMI\000\
  COLON\000\
  PRINT\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  ASSIGN\000\
  EOF\000\
  IN\000\
  RANGE\000\
  APPEND\000\
  POP\000\
  NOT\000\
  OR\000\
  AND\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  FOR\000\
  WHILE\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  THEN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULO\000\
  DEF\000\
  RETURN\000\
  "

let yynames_block = "\
  STRING\000\
  ID\000\
  LITERAL\000\
  BLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 41 "src/Rayquaza_parser.mly"
            ( Program(_1) )
# 387 "src/Rayquaza_parser.ml"
               : Rayquaza_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 44 "src/Rayquaza_parser.mly"
         ( [ _1 ] )
# 394 "src/Rayquaza_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 45 "src/Rayquaza_parser.mly"
                   ( _1 :: _2 )
# 402 "src/Rayquaza_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "src/Rayquaza_parser.mly"
            ( Expr(_1) )
# 409 "src/Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 51 "src/Rayquaza_parser.mly"
                            ( Block(_2) )
# 416 "src/Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 52 "src/Rayquaza_parser.mly"
                                            ( If(_3, _5, Block([])) )
# 424 "src/Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 53 "src/Rayquaza_parser.mly"
                                         ( If(_3, _5, _7) )
# 433 "src/Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 54 "src/Rayquaza_parser.mly"
                                  ( While(_3, _5) )
# 441 "src/Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 55 "src/Rayquaza_parser.mly"
                     ( Return(_2) )
# 448 "src/Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'formals_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 56 "src/Rayquaza_parser.mly"
                                                              ( Func(_2, _4, _7) )
# 457 "src/Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "src/Rayquaza_parser.mly"
     ( [_1] )
# 464 "src/Rayquaza_parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 60 "src/Rayquaza_parser.mly"
                          ( _1::_3 )
# 472 "src/Rayquaza_parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "src/Rayquaza_parser.mly"
          ( Literal(_1) )
# 479 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 64 "src/Rayquaza_parser.mly"
         ( BoolLit(_1) )
# 486 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "src/Rayquaza_parser.mly"
       ( Var(_1) )
# 493 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "src/Rayquaza_parser.mly"
           ( StringLiteral(_1) )
# 500 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "src/Rayquaza_parser.mly"
                             ( Call("print", [_3]) )
# 507 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "src/Rayquaza_parser.mly"
                   ( Binop(_1, Add, _3) )
# 515 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "src/Rayquaza_parser.mly"
                    ( Binop(_1, Sub, _3) )
# 523 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "src/Rayquaza_parser.mly"
                     ( Binop(_1, Div, _3) )
# 531 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "src/Rayquaza_parser.mly"
                    ( Binop(_1, Mul, _3) )
# 539 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "src/Rayquaza_parser.mly"
                     ( Binop(_1, Mod, _3) )
# 547 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "src/Rayquaza_parser.mly"
                 ( Binop(_1, Equal, _3) )
# 555 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "src/Rayquaza_parser.mly"
                  ( Binop(_1, Neq, _3) )
# 563 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "src/Rayquaza_parser.mly"
                 ( Binop(_1, Less, _3) )
# 571 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "src/Rayquaza_parser.mly"
                 ( Binop(_1, Greater, _3) )
# 579 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "src/Rayquaza_parser.mly"
                  ( Binop(_1, And, _3) )
# 587 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "src/Rayquaza_parser.mly"
                 ( Binop(_1, Or, _3) )
# 595 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 79 "src/Rayquaza_parser.mly"
                                ( ArrayLit(_2) )
# 602 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "src/Rayquaza_parser.mly"
                                ( ArrayAccess(_1, _3) )
# 610 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "src/Rayquaza_parser.mly"
                                            ( ArrayAssign(_1, _3, _6) )
# 619 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "src/Rayquaza_parser.mly"
                   ( Assign(_1, _3) )
# 627 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 83 "src/Rayquaza_parser.mly"
                       ( _2 )
# 634 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 84 "src/Rayquaza_parser.mly"
                              ( Call(_1, _3) )
# 642 "src/Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "src/Rayquaza_parser.mly"
                ( [] )
# 648 "src/Rayquaza_parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 88 "src/Rayquaza_parser.mly"
         ( _1 )
# 655 "src/Rayquaza_parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "src/Rayquaza_parser.mly"
       ( [_1] )
# 662 "src/Rayquaza_parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 92 "src/Rayquaza_parser.mly"
                    ( _1 :: _3 )
# 670 "src/Rayquaza_parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "src/Rayquaza_parser.mly"
              ( [] )
# 676 "src/Rayquaza_parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list_nonempty) in
    Obj.repr(
# 96 "src/Rayquaza_parser.mly"
                       ( _1 )
# 683 "src/Rayquaza_parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "src/Rayquaza_parser.mly"
       ( [_1] )
# 690 "src/Rayquaza_parser.ml"
               : 'expr_list_nonempty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list_nonempty) in
    Obj.repr(
# 100 "src/Rayquaza_parser.mly"
                                  ( _1 :: _3 )
# 698 "src/Rayquaza_parser.ml"
               : 'expr_list_nonempty))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Rayquaza_ast.program)
