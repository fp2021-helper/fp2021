open OcamlAdt_lib.Interpret
open Format

let () =
  printf "%a" pp_run_interpret
    {|
  let x = function
  | int -> true
  | string -> false

  let y = x 2
|}
