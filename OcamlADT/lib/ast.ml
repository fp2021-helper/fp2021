type name = string

type const =
  | Bool of bool
  | Int of int
  | Nill
  | Unit
[@@deriving show]

type pattern =
  | PConst of const
  | PVar of name
  | PCons of pattern * pattern
[@@deriving show]

type binop =
  | Plus
  | Minus
  | Mult
  | Divide
  | And
  | Or
[@@deriving show]

type expr =
  | Const of const
  | BinOp of binop * expr
  | Var of name
  | Fun of name * expr
  | Cons of expr * expr
  | Let of name * expr * expr
  | LetRec of name * expr * expr
[@@deriving show]
