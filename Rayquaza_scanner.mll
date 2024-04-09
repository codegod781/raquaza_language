(* OCamllex scanner for Rayquaza *)

{ open Rayquaza_parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  [' ' '\t' '\n'] { token lexbuf }
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
| "for"     { FOR }
| "return"  { RETURN }
| "def"  { DEF }
| digit+ as lem  { LITERAL(int_of_string lem) }
(*| "True"    { TRUE }
| "False"   { FALSE }*) (*Edited out until implemented*)
(*| "\""      { STRING (read_string lexbuf false) } *)
| ['a'-'z' 'A'-'Z' '_'] (['a'-'z' 'A'-'Z' '0'-'9' '_'])* as id { ID id }
| eof       { EOF }
| _ as c    { raise (Failure ("illegal character " ^ Char.escaped c)) }

(*and read_string lexbuf started =
  if started then
    match%sedlex lexbuf with
    | "\"" -> ""
    | eof -> raise (Failure "EOF in string")
    | any -> let char = Sedlexing.Utf8.lexeme lexbuf in
             char ^ read_string lexbuf true
    | _ -> raise (Failure "String not terminated")
  else
    match%sedlex lexbuf with
    | "\"" -> read_string lexbuf true
    | _ -> raise (Failure "String not started correctly")
    *)
