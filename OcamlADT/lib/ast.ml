type id = string [@@deriving show {with_path= false}]

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
[@@deriving show {with_path= false}]

and const =
  | CBool of bool (*  true  *)
  | CInt of int (*  1     *)
  | CString of string (*  "qwe" *)
[@@deriving show {with_path= false}]

and case = pattern * expr
(* | _ :: [] -> 1 *)

and pattern =
  | PWild (* _        *)
  | PConst of const (* 1        *)
  | PVar of id (* abc      *)
  | PCons of pattern * pattern (* hd :: tl *)
  | PList of pattern list (* [a; b]   *)
[@@deriving show {with_path= false}]

and expr =
  | EConst of const (* 1                     *)
  | EBinOp of bin_op * expr * expr (* 1 + 1                 *)
  | EVar of id (* abc                   *)
  | EList of expr list (* [1; 2]                *)
  | ETuple of expr list (* 1, 2                  *)
  | ECons of expr * expr (* hd :: tl              *)
  | EIf of expr * expr * expr (* if true then 1 else 0 *)
  | ELet of id * expr * expr (* let x e in e'         *)
  | EFun of pattern * expr (* fun x -> x * 2        *)
  | EApp of expr * expr (* f x                   *)
  | EMatch of expr * case list (* match e with | _ -> 0 *)
[@@deriving show {with_path= false}]

and acase = id * const
(* | Number of int *)

and decl =
  | DLet of pattern * expr (* let x = 1                             *)
  | DAdt of id * acase list
(* type ee = Age of int | Name of string *)
