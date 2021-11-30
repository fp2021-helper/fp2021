type id = string

type bin_op =
  | Add (*  +   *)
  | Sub (*  -   *)
  | Mul (*  *   *)
  | Div (*  /   *)
  | Less (*  <   *)
  | Leq (*  <=  *)
  | Gre (*  >   *)
  | Geq (*  >=  *)
  | Eq (*  ==  *)
  | Neq (*  <>  *)
  | And (*  &&  *)
  | Or (*  ||  *)
[@@deriving show { with_path = false }]

and const =
  | CBool of bool (*   true  *)
  | CInt of int (*    1    *)
  | CString of string (*  "abc"  *)
[@@deriving show { with_path = false }]

and case = pat * exp
(* | _ :: [] -> 5 *)

and pat =
  | PWild (* _        *)
  | PConst of const (* 1        *)
  | PVar of id (* abc      *)
  | PCons of pat * pat (* hd :: tl *)
  | PList of pat list (* [a; b]   *)
  | PTuple of pat list (* a, b     *)
[@@deriving show { with_path = false }]

and exp =
  | EConst of const
  | EBinOp of bin_op * exp * exp
  | EFun of id * exp
  | ECons of exp * exp
  | ELet of id * exp * exp
  | ELetRec of id * exp * exp
  | App of exp * exp
  | EType of id * const
  | EAdt of id * exp list
  | EMatch of exp * (pat * exp) list
[@@deriving show { with_path = false }]
