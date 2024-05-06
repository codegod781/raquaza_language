(*open Rayquaza_ast*)

type action = Ast | Sast | LLVM_IR

let _ = 
  let action = ref LLVM_IR in
  let channel = ref stdin in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
  ] in
  let usage_msg = "usage: ./Rayuaza.native [-a|-s|-l] [file.mc]" in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel stdin in
  let ast = Rayquaza_parser.program Rayquaza_scanner.token lexbuf in
  match !action with
    Ast -> print_string (Rayquaza_ast.string_of_program ast)
    | Sast ->
    (* Temporarily bypass semantic analysis until Rayquaza_semant is implemented *)
    (* let sast = Rayquaza_semant.check ast in
       print_string (Rayquaza_sast.string_of_sprogram sast) *)
    print_string "(Semantic analysis not implemented)"
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))



 (* let program = Rayquaza_parser.program Rayquaza_scanner.token lexbuf in
  print_endline (string_of_program program)*)
