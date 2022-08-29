open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let ( >> ) x f = x >>= fun _ -> f
  let return = Result.ok
  let error = Result.error
end

type table_constructor = {
  key : string;
  arguments : (types * names) list;
  call_constructor : expressions option;
  body : statements;
}
[@@deriving show { with_path = false }]

type table_field = {
  field_type : types;
  field_modifiers : modifiers list;
  key : string;
  sub_tree : expressions option;
}
[@@deriving show { with_path = false }]

type table_method = {
  method_type : types;
  method_modifiers : modifiers list;
  arguments : (types * names) list;
  key : string;
  interface_key : string option;
  body : statements option;
  is_overriden : bool;
}
[@@deriving show { with_path = false }]

type table_class = {
  this_key : string;
  fields_table : table_field KeyMap.t;
  methods_table : table_method KeyMap.t;
  constructors_table : table_constructor KeyMap.t;
  children_keys : string list;
  is_abstract : bool;
  is_static : bool;
  is_class : bool;
  parent_key : string list;
  decl_tree : objects;
}
[@@deriving show { with_path = false }]
