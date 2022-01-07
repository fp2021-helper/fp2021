open Format
open Ast
open Pretty_printer
open Parser
open Angstrom

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module BindsMap = Map.Make (String)

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | Vtuple of value list
  | VFun of pattern * expr * env

and env = value BindsMap.t

exception Not_bound

let look_for_bind map name =
  try BindsMap.find name map with Not_found -> raise Not_bound

let vint i = VInt i
let vstring s = VString s
let vbool b = VBool b
let vtuple t = Vtuple t
let vlist l = VList l

type run_error =
  | Division_by_zero
  | Unbound of string
  | Match_exhaust of pattern list
  | Incorrect_eval of value
  | Match_fail
[@@deriving show {with_path= false}]

type run_ok_element = string * (tyexpr * value)
type run_ok = run_ok_element list

(* pretty printers *)
let rec pp_value _ = function
  | VInt i -> printf "%d" i
  | VString str -> printf "%s" str
  | VBool b -> printf "%b" b
  | Vtuple t ->
      printf "(%a)" (pp_print_list ~pp_sep:(fun _ _ -> printf ", ") pp_value) t
  | VList l ->
      printf "[%a]" (pp_print_list ~pp_sep:(fun _ _ -> printf "; ") pp_value) l
  | VFun _ -> printf "<fun>"

let pp_error = function
  | Unbound str -> printf "Unbound value %s" str
  | Division_by_zero -> printf "Division by zero"
  | Match_exhaust _ -> printf "This pattern matching is not exhaustive"
  | Incorrect_eval value -> printf "Value %a has incorrect type" pp_value value
  | _ -> printf "Error with interpret"

let pp_run_ok_element fmt (name, (tyexpr, value)) =
  fprintf fmt "val %s : %a = %a" name pp_tyexpr tyexpr pp_value value

let pp_run_ok = pp_print_list ~pp_sep:pp_force_newline pp_run_ok_element

module Interpret (M : MONAD_FAIL) = struct
  open M

  let lookup name env = 
    match Option.bind (BindsMap.find_opt name env) ( ! ) with
    | None -> fail (Unbound name)
    | Some s -> return s

  let add_val name value env = BindsMap.add name (ref (Some value)) env

  let extend_env env binds = 
    List.fold_left (fun env (id, v) -> BindsMap.add id v env) env binds

  let rec case_env pattern value = 
    match pattern, value with
    | PNil, VList [] -> return []
    | PWild, _ -> return []
    | PVar name, v -> return [ name, v ]
    | PConst (CInt c), VInt v ->
      if c = v then return [] else fail (Match_fail)
    | PConst (CString c), VString v ->
      if c = v then return [] else fail (Match_fail)
    | PConst (CBool c), VBool v -> 
      if c = v then return [] else fail (Match_fail)
    | PCons (patt1, patt2), VList (hd :: tl) ->
      case_env patt1 hd >>= (fun hd_match ->
      case_env patt2 (VList tl) >>= (fun tl_match ->
      return ( hd_match @ tl_match )))
    | PTuple pts, Vtuple vts ->
      (match pts, vts with
      | [], [] -> return []
      | hd_pts :: tl_pts, hd_vts :: tl_vts ->
        case_env hd_pts hd_vts >>= (fun bind_hd -> 
        case_env (PTuple tl_pts) (Vtuple tl_vts) >>= (fun bind_tl ->
        return (bind_hd @ bind_tl )))
      | _ -> fail (Match_fail))
    | _ -> return []

  let rec eval expr env = 
    match expr with
    | EVar name -> look_for_bind env name
    | EConst c ->
      (match c with
      | CInt i -> return (vint i)
      | CString str -> return (vstring str)
      | CBool b -> return (vbool b))
    | ENil -> return (vlist [])
    | EBinOp (op, e1, e2) ->
      let* value1 = eval e1 env in
      (match value1, op with
      | VBool false, And -> return (vbool false)
      | VBool true, Or -> return (vbool true)
      | _ ->
        let* value2 = eval e2 env in 
        (match value1, value2, op with 
        | VInt x, VInt y, Add -> return (vint (x + y))
        | VInt x, VInt y, Sub -> return (vint (x - y))
        | VInt x, VInt y, Mul -> return (vint (x * y))
        | VInt _, VInt y, Div when y = 0 -> fail Division_by_zero
        | VInt x, VInt y, Div -> return (vint (x / y))
        | VInt x, VInt y, Less -> return (vbool (x < y))
        | VInt x, VInt y, Leq -> return (vbool (x <= y))
        | VInt x, VInt y, Gre -> return (vbool (x > y))
        | VInt x, VInt y, Geq -> return (vbool (x >= y))
        | VInt x, VInt y, Eq -> return (vbool (x = y))
        | VString str1, VString str2, Eq -> return (vbool (str1 = str2))
        | VBool b1, VBool b2, Eq -> return (vbool (b1 = b2))
        | VInt x, VInt y, Neq -> return (vbool (x != y))
        | VString str1, VString str2, Neq -> return (vbool (str1 != str2))
        | VBool b1, VBool b2, Neq -> return (vbool (b1 != b2))
        | VBool _, VBool b2 , And 
        | VBool _, VBool b2, Or -> return (vbool b2)
        | _ -> fail (Incorrect_eval (Vtuple [ value1; value2 ]))))
    | ETuple l -> all (List.map (fun e -> eval e env) l) >>| vtuple
    | EList _ -> return (vlist [])
    | EIf (e1, e2, e3) -> 
      let* res = eval e1 env in
      (match res with
      | VBool true -> eval e2 env
      | VBool false -> eval e3 env
      | e -> fail (Incorrect_eval e))
    | EFun (pattern, expr) -> return (VFun (pattern, expr, env))
    | ELet (* TODO *)
    | EMatch
    | EApp (* TODO *) -> return (VInt 2)

  let run program = 
    List.fold_left 
    (fun acc decl -> acc >>= (env, vals) ->
    add_decl decl env
    >>= fun env -> lookup decl.name )
  let pp_res fmt res = 
end
