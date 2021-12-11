open Angstrom
open Ast
open Base

let parse p s = parse_string ~consume:All p s

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e 
  >>= go <|> return acc in
  e >>= fun init -> go init

let rec chainr1 e op =
   e >>= fun a -> op 
   >>= (fun f -> chainr1 e op >>| f a) 
   <|> return a

let is_ws = function
  | ' ' | '\t'  -> true
  | _          -> false

let is_eol = function
  | '\r' | '\n' -> true
  | _           -> false

let is_digit = function
  | '0' .. '9' -> true
  | _          -> false

let is_id = function
| '0' .. '9' | 'a' .. 'z' 
| 'A' .. 'Z' | '_'        -> true
| _                       -> false 

let is_keyword = function
  | "let"   | "rec"   | "fun"
  | "true"  | "false" | "in"
  | "if"    | "then"  | "else"
  | "match" | "with"  | "function"
  | "type"  | "of" -> true
  | _    -> false

let empty = take_while (fun c -> is_ws c || is_eol c)
let empty1 = take_while1 (fun c -> is_ws c || is_eol c)
let token s = empty *> string s
let trim p = empty *> p <* empty
let kwd s = token s <* empty1
let between l r p = l *> p <* r

(* braces *)
let lp = token "("
let rp = token ")"
let lsb = token "["
let rsb = token "]"
let comma = token ","
let colon = token ":"
let semi = token ";"
let semisemi = token ";;"
let bar = token "|"
let arrow = token "->"
let parens p = lp *> p <* rp

(********************** constructors **********************)
(* const constructors *)

let cint n = CInt n
let cbool b = CBool b
let cstring s = CString s

(* expr constructors *)

let econst c = EConst c
let evar id = EVar id
let elist l = EList l 
let etuple pl = ETuple pl
let econs e1 e2 = ECons (e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elet id e1 e2 = ELet (id, e1, e2)
let efunction cases = EFun(PVar "match", EMatch (EVar "match", cases))
let eapp = return (fun e1 e2 -> EApp(e1, e2))
let ematch e cases = EMatch(e, cases)
let eop o e1 e2 = EBinOp(o, e1, e2)
let elist = List.fold_right ~f:econs ~init:ENil
let efun args rhs = 
  let helper p e = EFun(p, e) in
  List.fold_right args ~f:helper ~init:rhs

(* case constructors *)

let ccase p e = (p, e)
let acase id c = (id, c)

(* binding constructors *)

let bbind isrec p e = (isrec, p, e)

(* pattern constructors *)

let pwild _ = PWild
let pvar id = PVar id
let pconst c = PConst c
let ptuple pl = PTuple pl
let popcons = token "::" *> return (fun p1 p2 -> PCons (p1, p2))
let pcons = return (fun p1 p2 -> PCons (p1, p2))
let plist = List.fold_right ~f:(fun p1 p2 -> PCons (p1, p2)) ~init:PNil

(* decl constructors *)

let dlet isrec p e = DLet (isrec, p, e)
let dadt id acases = DAdt(id, acases)

(* plain parsers *)

let choice_op ops =
  choice ( List.map ~f:(fun (tok, cons) -> token tok *> (return @@ eop cons)) ops )

let add_sub = choice_op [ "+", Add; "-", Sub ]
let mult_div = choice_op [ "*", Mul; "/", Div ]
let cmp = choice_op [ ">=", Geq; ">", Gre; "<=", Leq; "<", Less ]
let eq_uneq = choice_op [ "=", Eq; "<>", Neq ]
let conj = choice_op [ "&&", And ]
let disj = choice_op [ "||", Or ]
let cons = token "::" *> return econs

let identifier is_valid_first_char = 
  let* fst = empty *> satisfy is_valid_first_char in
  let take_func = 
    match fst with
    | '_' -> many1
    | _ -> many
  in
  let* inner = take_func ( satisfy is_id ) in
  let id = Base.String.of_char_list ( fst :: inner ) in
  if is_keyword id then fail "Ivalid id" else return id

let id = 
  let is_valid_first_char = function 
  | 'a' .. 'z' | '_' -> true 
  | _                -> false
  in
  identifier is_valid_first_char 
  
(* const parsing *)

let uns = trim ( take_while1 is_digit )

let cunsint = 
  let* helper = uns in
  return ( Int.of_string helper ) >>| cint

let cint = 
  let* helper = uns in
  let* sign = option "" (token "+" <|> token "-") in
  return ( Int.of_string (sign ^ helper) ) >>| cint

let cbool = 
  let btrue = kwd "true" *> return (cbool true) in
  let bfalse = kwd "false" *> return (cbool false) in
  btrue <|> bfalse

let cstring = 
  between (char '"') (char '"') @@
  take_while (function 
    | '"' -> false
    | _   -> true)
  >>| cstring

let const = trim ( choice [ cint; cbool; cstring ] )
let uns_const = trim ( choice [ cunsint; cbool; cstring ] )

(* pattern parsing *)
let pvar = id >>| pvar
let pwild = token "_" >>| pwild
let pconst = const >>| pconst

type pdispatch =
  { tuple: pdispatch -> pattern t;
    cons: pdispatch -> pattern t;
    pat: pdispatch -> pattern t
  }

(*type pdispatch =
  { adt: pdispatch -> pattern t;
    tuple: pdispatch -> pattern t;
    cons: pdispatch -> pattern t;
    pat: pdispatch -> pattern t
  }*)

let pack = 
  let pat d = 
    fix (fun _self ->
      trim (choice [ d.tuple d; d.cons d ]) )
  in
  let tuple d = 
    fix (fun _self -> 
      trim @@ lift2 (fun hd tl -> hd :: tl) (d.cons d) (many1 (comma *> d.cons d))
      >>| ptuple)
  in
  let cons d = 
    fix (fun _self -> 
      let plist = trim ( between lsb rsb ( sep_by semi ( d.pat d ) ) ) >>| plist in
      let prim = trim @@ choice [ pconst; pvar; pwild; plist; parens @@ d.pat d ] in
      trim @@ chainr1 prim popcons)
  in
  { tuple; cons; pat }

let pattern = pack.pat pack

(* expr parsing *)

type edispatch = 
  { key: edispatch -> expr t;
    tuple: edispatch -> expr t;
    expr: edispatch -> expr t;
    op: edispatch -> expr t
  }

let pack = 
  let key d = 
    fix 
    @@ fun _self -> 
    let eif = 
      trim 
      @@ lift3 eif (kwd "if" *> d.expr d) (kwd "then" *> d.expr d) (kwd "else" *> d.expr d)
    in
    let exp d = fix @@ fun _self -> trim @@ d.key d <|> d.tuple d <|> d.op d in

