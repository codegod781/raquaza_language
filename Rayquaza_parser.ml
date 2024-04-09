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
  | EOF
  | NOT
  | OR
  | AND
  | EQ
  | NEQ
  | LT
  | FOR
  | WHILE
  | IF
  | ELSE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | DEF
  | RETURN
  | STRING of (
# 14 "Rayquaza_parser.mly"
        string
# 34 "Rayquaza_parser.ml"
)
  | ID of (
# 16 "Rayquaza_parser.mly"
        string
# 39 "Rayquaza_parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "Rayquaza_parser.mly"
open Rayquaza_ast
# 46 "Rayquaza_parser.ml"
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
    0 (* EOF *);
  267 (* NOT *);
  268 (* OR *);
  269 (* AND *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LT *);
  273 (* FOR *);
  274 (* WHILE *);
  275 (* IF *);
  276 (* ELSE *);
  277 (* PLUS *);
  278 (* MINUS *);
  279 (* TIMES *);
  280 (* DIVIDE *);
  281 (* MODULO *);
  282 (* DEF *);
  283 (* RETURN *);
    0|]

let yytransl_block = [|
  284 (* STRING *);
  285 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\005\000\005\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\001\000\002\000\003\000\007\000\005\000\003\000\001\000\002\000\
\001\000\001\000\004\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\000\000\026\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\003\000\000\000\000\000\006\000\000\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\013\000\
\011\000\000\000\000\000\000\000\021\000\005\000\000\000\025\000\
\000\000\004\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\039\000\040\000"

let yysindex = "\002\000\
\025\255\000\000\003\255\002\255\025\255\005\255\012\255\002\255\
\000\000\004\255\000\000\000\000\025\255\061\255\002\255\089\255\
\011\255\002\255\002\255\077\255\002\255\002\255\000\000\000\000\
\002\255\002\255\002\255\002\255\002\255\002\255\002\255\101\255\
\000\000\000\000\113\255\125\255\000\000\171\255\030\255\000\000\
\191\255\195\255\049\255\016\255\016\255\020\255\000\000\000\000\
\000\000\025\255\025\255\002\255\000\000\000\000\019\255\000\000\
\025\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\255\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\046\255\000\000\000\000\
\144\255\035\255\189\255\159\255\165\255\143\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\218\255\252\255\000\000\004\000"

let yytablesize = 266
let yytable = "\016\000\
\007\000\017\000\001\000\020\000\003\000\004\000\015\000\021\000\
\018\000\023\000\032\000\054\000\055\000\035\000\036\000\019\000\
\038\000\041\000\058\000\034\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\003\000\004\000\009\000\010\000\029\000\
\005\000\022\000\053\000\018\000\030\000\031\000\057\000\018\000\
\030\000\031\000\006\000\007\000\018\000\009\000\018\000\038\000\
\022\000\009\000\024\000\008\000\009\000\010\000\009\000\056\000\
\009\000\009\000\009\000\009\000\009\000\024\000\027\000\028\000\
\029\000\009\000\009\000\000\000\000\000\030\000\031\000\000\000\
\025\000\026\000\027\000\028\000\029\000\037\000\000\000\000\000\
\000\000\030\000\031\000\000\000\000\000\000\000\000\000\000\000\
\025\000\026\000\027\000\028\000\029\000\033\000\000\000\000\000\
\000\000\030\000\031\000\000\000\025\000\026\000\027\000\028\000\
\029\000\049\000\000\000\000\000\000\000\030\000\031\000\000\000\
\025\000\026\000\027\000\028\000\029\000\050\000\000\000\000\000\
\000\000\030\000\031\000\000\000\025\000\026\000\027\000\028\000\
\029\000\051\000\000\000\000\000\000\000\030\000\031\000\000\000\
\025\000\026\000\027\000\028\000\029\000\000\000\000\000\016\000\
\019\000\030\000\031\000\016\000\019\000\000\000\000\000\000\000\
\016\000\019\000\016\000\016\000\016\000\016\000\016\000\014\000\
\000\000\000\000\000\000\014\000\000\000\015\000\000\000\000\000\
\014\000\015\000\014\000\014\000\014\000\014\000\015\000\000\000\
\015\000\015\000\015\000\015\000\052\000\000\000\025\000\026\000\
\027\000\028\000\029\000\000\000\000\000\017\000\000\000\030\000\
\031\000\017\000\000\000\000\000\000\000\000\000\017\000\000\000\
\017\000\017\000\025\000\026\000\027\000\028\000\029\000\026\000\
\027\000\028\000\029\000\030\000\031\000\000\000\000\000\030\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000"

let yycheck = "\004\000\
\000\000\005\000\001\000\008\000\003\001\004\001\004\001\004\001\
\004\001\013\000\015\000\050\000\051\000\018\000\019\000\004\001\
\021\000\022\000\057\000\009\001\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\003\001\004\001\028\001\029\001\016\001\
\008\001\030\001\005\001\001\001\021\001\022\001\020\001\005\001\
\021\001\022\001\018\001\019\001\010\001\001\001\012\001\052\000\
\005\001\005\001\005\001\027\001\028\001\029\001\010\001\052\000\
\012\001\013\001\014\001\015\001\016\001\001\001\014\001\015\001\
\016\001\021\001\022\001\255\255\255\255\021\001\022\001\255\255\
\012\001\013\001\014\001\015\001\016\001\001\001\255\255\255\255\
\255\255\021\001\022\001\255\255\255\255\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\005\001\255\255\255\255\
\255\255\021\001\022\001\255\255\012\001\013\001\014\001\015\001\
\016\001\005\001\255\255\255\255\255\255\021\001\022\001\255\255\
\012\001\013\001\014\001\015\001\016\001\005\001\255\255\255\255\
\255\255\021\001\022\001\255\255\012\001\013\001\014\001\015\001\
\016\001\005\001\255\255\255\255\255\255\021\001\022\001\255\255\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\001\001\
\001\001\021\001\022\001\005\001\005\001\255\255\255\255\255\255\
\010\001\010\001\012\001\013\001\014\001\015\001\016\001\001\001\
\255\255\255\255\255\255\005\001\255\255\001\001\255\255\255\255\
\010\001\005\001\012\001\013\001\014\001\015\001\010\001\255\255\
\012\001\013\001\014\001\015\001\010\001\255\255\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\001\001\255\255\021\001\
\022\001\005\001\255\255\255\255\255\255\255\255\010\001\255\255\
\012\001\013\001\012\001\013\001\014\001\015\001\016\001\013\001\
\014\001\015\001\016\001\021\001\022\001\255\255\255\255\021\001\
\022\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\009\001"

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
  EOF\000\
  NOT\000\
  OR\000\
  AND\000\
  EQ\000\
  NEQ\000\
  LT\000\
  FOR\000\
  WHILE\000\
  IF\000\
  ELSE\000\
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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 31 "Rayquaza_parser.mly"
              ( Program(_1) )
# 247 "Rayquaza_parser.ml"
               : Rayquaza_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 34 "Rayquaza_parser.mly"
                                       ( Expr(_1) )
# 254 "Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 35 "Rayquaza_parser.mly"
                                       ( Block _2 )
# 261 "Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 36 "Rayquaza_parser.mly"
                                         ( If(_3, _5, _7) )
# 270 "Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 37 "Rayquaza_parser.mly"
                                       ( While(_3, _5) )
# 278 "Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 38 "Rayquaza_parser.mly"
                                       ( Return _2 )
# 285 "Rayquaza_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 41 "Rayquaza_parser.mly"
         ( [ _1 ] )
# 292 "Rayquaza_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 42 "Rayquaza_parser.mly"
                   ( _1 :: _2 )
# 300 "Rayquaza_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "Rayquaza_parser.mly"
                                       ( Var _1 )
# 307 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "Rayquaza_parser.mly"
                                       ( StringLiteral _1 )
# 314 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 47 "Rayquaza_parser.mly"
                                      ( Call("print", [_3]) )
# 321 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "Rayquaza_parser.mly"
                                       ( Binop(_1, Add, _3) )
# 329 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "Rayquaza_parser.mly"
                                       ( Binop(_1, Sub, _3) )
# 337 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "Rayquaza_parser.mly"
                                       ( Binop(_1, Equal, _3) )
# 345 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "Rayquaza_parser.mly"
                                       ( Binop(_1, Neq, _3) )
# 353 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "Rayquaza_parser.mly"
                                       ( Binop(_1, Less, _3) )
# 361 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "Rayquaza_parser.mly"
                                       ( Binop(_1, And, _3) )
# 369 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "Rayquaza_parser.mly"
                                       ( Binop(_1, Or, _3) )
# 377 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "Rayquaza_parser.mly"
                                       ( Assign(_1, _3) )
# 385 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 56 "Rayquaza_parser.mly"
                                       ( _2 )
# 392 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 57 "Rayquaza_parser.mly"
                                       ( Call(_1, _3) )
# 400 "Rayquaza_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "Rayquaza_parser.mly"
                                       ( [] )
# 406 "Rayquaza_parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 61 "Rayquaza_parser.mly"
                                       ( _1 )
# 413 "Rayquaza_parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "Rayquaza_parser.mly"
                                       ( [_1] )
# 420 "Rayquaza_parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 65 "Rayquaza_parser.mly"
                                       ( _1 :: _3 )
# 428 "Rayquaza_parser.ml"
               : 'args))
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
