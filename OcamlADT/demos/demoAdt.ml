open OcamlADT_lib.Interpret
open Format

let () =
  printf "%a" pp_run_interpret
    {|
  type person = | Age of int | Name of string

  let misha = Age 2
|}
