open Ast
open Format

let rec pp_tyexpr fmt = function
  | TInt -> printf "int"
  | TString -> printf "string"
  | TBool -> printf "bool"
  | TTuple ts ->
      printf "(%a)"
        (pp_print_list
           ~pp_sep:(fun _ _ -> printf " * ")
           (fun fmt ty -> pp_tyexpr fmt ty) )
        ts
  | TList l -> printf "%a list" pp_tyexpr l
  | TArrow _ -> printf "->"
  | TAdt id -> printf "%s" id
