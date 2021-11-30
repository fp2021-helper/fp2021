type id = string

type bin_op =
  | Add  (*  +   *)
  | Sub  (*  -   *)
  | Mul  (*  *   *)
  | Div  (*  /   *)
  | Less (*  <   *)
  | Leq  (*  <=  *)
  | Gre  (*  >   *)
  | Geq  (*  >=  *)
  | Eq   (*  ==  *)
  | Neq  (*  <>  *)
  | And  (*  &&  *)
  | Or   (*  ||  *)
[@@deriving show { with_path = false }]

and const =
  | CBool of bool      (*   true  *)
  | CInt of int        (*    1    *)
  | CString of string  (*  "abc"  *)
[@@deriving show { with_path = false }]

and case = pat * exp   (* | _ :: [] -> 5 *)

and pat =
  | PWild               (* _        *)
  | PConst of const     (* 1        *)
  | PVar of id          (* abc      *)
  | PCons of pat * pat  (* hd :: tl *)
  | PList of pat list   (* [a; b]   *)
  | PTuple of pat list  (* a, b     *)
[@@deriving show { with_path = false }]

and exp =
  | EConst of const               (* 1                     *)
  | EBinOp of bin_op * exp * exp  (* 1 + 1                 *)
  | EVar of id                    (* abc                   *)
  | EList of exp list             (* [1; 2]                *)
  | ETuple of exp list            (* 1, 2                  *)
  | ECons of exp * exp            (* hd :: tl              *)
  | EIf of exp * exp * exp        (* if true then 1 else 0 *)
  | ELet of id * exp * exp        (* let x e in e'         *)
  | ELetRec of id * exp * exp     (* let rec x e in e'     *)
  | EFun of id * exp              (* fun x -> x * 2        *)
  | App of exp * exp              (* f x                   *)
  | EMatch of exp * case list     (* match e with | _ -> 0 *)
[@@deriving show { with_path = false }]
