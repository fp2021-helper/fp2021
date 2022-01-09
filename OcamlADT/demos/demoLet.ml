open OcamlAdt_lib.Interpret
open Format

let () = printf "%a" pp_run_interpret {|
  let x y = y + 2
  let z = x 2
|}
