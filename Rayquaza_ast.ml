type operator = Add | Sub | Mul | Div | Equal | Neq | Less | And | Or

type typ = Int | Float | String | Bool

type expr =
  | Literal of int
  | FloatLit of float
  | StringLiteral of string
  | BoolLit of bool
  | Var of string
  | Binop of expr * operator * expr
  | Assign of string * expr
  | Call of string * expr list
  | Seq of expr * expr
     
type stmt =
  | Expr of expr
  | Return of expr
  | FunctionDef of typ * string * (typ * string) list * stmt list
  | If of expr * stmt * stmt
  | IfNoElse of expr * stmt
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


let string_of_program (prog) =
  "\n\nParsed program: \n\n"  (* ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) *)
