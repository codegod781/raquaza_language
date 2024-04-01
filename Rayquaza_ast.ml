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
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | FunctionDef of typ * string * (typ * string) list * stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt


type program =
  | Program of stmt list