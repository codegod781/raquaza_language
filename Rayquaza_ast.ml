type operator = Add | Sub | Mul | Div | Equal | Neq | Less | And | Or

type typ = Int | Float | String | Bool

type expr =
  | Literal of int
  | FloatLit of float
  | StringLiteral of string
  | BoolLit of bool
  | Id of string
  | Var of string
  | Binop of expr * operator * expr
  | Assign of string * expr
  | Call of string * expr list
  | Seq of expr * expr
     
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  (* | FunctionDef of typ * string * (typ * string) list * stmt list *)
  | If of expr * stmt * stmt
  (* | IfNoElse of expr * stmt *)
  | While of expr * stmt
  | Func of string * (string list) * (stmt list)


(* 
 * def foo(x, y, z):
   {
      statements
   }
 *)

type program =
  | Program of stmt list


(* let string_of_program (prog) =
  "\n\nParsed program: \n\n"  (* ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) *) *)


(* type program = bind list * func_def list *)

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "and"
  | Or -> "or"
  | Mul -> "*"
  | Div -> "/"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | StringLiteral(s) -> "\"" ^ s ^ "\""
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Var(v) -> v
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Seq(e1, e2) ->
    string_of_expr e1 ^ "; " ^ string_of_expr e2

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  (* | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2 *)
  (* | IfNoElse(e, s) -> "if (" ^ string_of_expr e ^ ") " ^ string_of_stmt s *)
  (* | If(e, s1, elifs, else_opt) ->
    let if_part = "if (" ^ string_of_expr e ^ ") " ^ string_of_stmt s1 in
    let elif_parts = List.map (fun (cond, stmt) ->
      "elif (" ^ string_of_expr cond ^ ") " ^ string_of_stmt stmt) elifs in
    let else_part = match else_opt with
      | None -> ""
      | Some s -> "else " ^ string_of_stmt s
    in
    if_part ^ "\n" ^ String.concat "\n" elif_parts ^ "\n" ^ else_part *)
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
            string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Func(name, args, body) ->
    "def " ^ name ^ "(" ^ String.concat ", " args ^ ") {\n" ^
    String.concat "" (List.map string_of_stmt body) ^ "}\n"

let string_of_vdecl (t, id) = id ^ ";\n"

  let string_of_program = function
  Program(stmts) ->
  "\n\nParsed program: \n\n" ^
  String.concat "\n" (List.map string_of_stmt stmts)