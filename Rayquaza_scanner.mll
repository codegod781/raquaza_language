(* OCalmlex scannner for Rayquaza *)

{ open Parser}

let letters = ['a'-'z' 'A'-'Z']
let digits = ['0'-'9']

(* 

print("Hello, World!");

*)

rule token = parse
  [' ' '\t' '\n'] { token lexbuf }
(* |"/*" { comment lexbuf } *)
| "print"   { PRINT }
| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| '['       { LBRACKET } 
| ']'       { RBRACKET }
| ';'       { SEMI }
| ','       { COMMA }
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| '%'       { MODULO }
| '='       { ASSIGN }
| "=="      { EQ }
| "!="      { NEQ }
| '<'       { LT }
| "and"     { AND }
| "or"      { OR }
| "if"      { IF }
| "else"    { ELSE }
| "while"   { WHILE }
| "for"     {FOR}
(* RETURN *)
| "return"  { RETURN }
| "True"    { BLIT(true)  }
| "False"   { BLIT(false) }
| letter (digit | letter | '_')* as id { ID (Lexing.lexeme lexbuf) } (* ID for any string combination of letter *)
| eof { EOF }
| _ as c { raise (Failure ("illegal character " ^ Char.escaped c)) }

(*

and comment = parse
  "*/" { token lexbuf }

*)
