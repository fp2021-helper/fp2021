open Ast

module KeyMap = struct
  include Map.Make (String)

  let pp pp_v ppf map =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) map;
    Format.fprintf ppf "@]]@]"
end

type field_references = {
  key : string;
  field_type : types;
  field_value : values;
  is_const : bool;
  assignments_count : int;
}

and method_reference = {
  method_key : string;
  method_type : types;
  args : (types * string) list;
  body : statements;
}
[@@deriving show { with_path = false }]

and object_references =
  | NullObjectReference
  | ObjectReference of {
      class_key : string;
      field_references_table : field_references KeyMap.t;
      method_references_table : method_reference KeyMap.t;
      parent_key : string option;
      dec_class : objects;
      number : int;
    }
