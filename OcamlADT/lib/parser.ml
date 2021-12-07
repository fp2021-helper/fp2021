open Angstrom
open Ast

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
let lsb = token "["
let rsb = token "]"
let comma = token ","
let colon = token ":"
let semi = token ";"
let semisemi = token ";;"
let bar = token "|"
let arrow = token "->"
let parens p = between lp rp p

(* tokens *)
let _then = token "then"
let _else = token "else"
let _if = token "if"
let _fun = token "fun"
let _in = token "in"
let _true = token "true"
let _false = token "false"
let _eol = token "\n"
let _rec = token "rec"
let _wild = token "_"
let _match = token "match"
let _bar = token "|"
let _with = token "with"
let _function = token "function"
let _type = token "type"

(********************** constructors **********************)
(* const constructors *)

let cint n = CInt n
let cbool b = CBool b
let cstring s = CString s

(* expr constructors *)

let econst c = EConst c
let evar id = EVar id
let elist l = EList l 
let etuple l = ETuple l
let econs e1 e2 = ECons (e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elet id e1 e2 = ELet (id, e1, e2)

