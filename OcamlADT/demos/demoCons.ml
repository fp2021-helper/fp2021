open OcamlAdt_lib.Interpret
open Format

let () = printf "%a" pp_run_interpret {|
  let x = 1 :: 2 :: []
|}
