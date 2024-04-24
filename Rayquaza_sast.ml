(* Sementacally-checked Abstract Syntax Tree and functions for printing it *)

Rayquaza_ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFloatLit of float
  | SStringLit of string
  | SBoolLit of bool
  | SId of string
  | SVar of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  (* Call *)
  | SCall of string * sexpr list
  | SSeq of sexpr * sexpr
  | SArrayLit of sexpr list
  | SArrayAccess of sexpr * sexpr
  | SArrayAssign of sexpr * sexpr * sexpr

type sstmt = 
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFunc of string * typ * bind list * sstmt 
  

  (* 
      type sfunc_def = {
        srtyp : typ;
        sfname : string;
        sformals : bind list;
        slocals : bind list;
        sbody : stmt list;
      } 
  *)

  type sprogram = 
    SProgram of stmt list

  let rec string_of_sexpr (t, e) = 
    "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SFloatLit(f) -> string_of_float f
      | SStringLit(s) -> s
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SId(s) -> s
      | SVar(s) -> v
      | SBinop(e1, o, e2) ->
          string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SSeq (e1, e2) -> 
          string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2
      | SArrayLit(el) -> "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
      | SArrayAccess(arr, idx) -> string_of_sexpr arr ^ "[" ^ string_of_sexpr idx ^ "]"
      | SArrayAssign(arr, idx, value) -> string_of_sexpr arr ^ "[" ^ string_of_sexpr idx ^ "] = " ^ string_of_sexpr value
    ) ^ ")"

  let rec string_of_sstmt = function
      SBlock(stmts) -> 
        "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
    | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
    | Return(sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n"
    | SIf(e, s, Block([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
    | SIf(e, s1, s2) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ 
                string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
    | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
    | Func(name, args, body) ->
        "def " ^ name ^ "(" ^ String.concat ", " args ^ ") {\n" ^ 
                String.concat "" (List.map string_of_sstmt body) ^ "}\n"

  (* let string_of_svdecl (t, id) = id ^ " :\n " ^ *)

  let string_of_sprogram (stmts) = 
    "\n\nSementically checked program: \n\n" ^
    String.concat "\n" (List.map string_of_sstmt stmts)
    

 