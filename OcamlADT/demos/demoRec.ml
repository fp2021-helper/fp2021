open OcamlADT_lib.Interpret
open Format

let () =
  printf "%a" pp_run_interpret
    {|
let rec map f = function
| [] -> []
| h :: tl -> f h :: map f tl

let x = [1;2;3]
let y = map (fun x -> x + 3) [1;2;3]
|}
