open Rayquaza_ast

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Rayquaza_parser.program Rayquaza_scanner.token lexbuf in
  print_endline (string_of_program program)