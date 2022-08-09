type modifiers =
  | Public
  | Private
  | Protected
  | Static
  | Const
  | Virtual
  | Override
  | Abstract
  | New
[@@deriving show { with_path = false }]

type types = TInt | TVoid | TString | TRef of string | TBool
[@@deriving show { with_path = false }]

type values =
  | VInt of int
  | VBool of bool
  | VString of string
  | VVoid
  | VObjectReference of object_references
[@@deriving show { with_path = false }]

and object_references =
  | NullObjectReference
  | ObjectReference of { class_key : string; parent_key : string; number : int }

type names = Name of string [@@deriving show { with_path = false }]

type expressions =
  | Add of expressions * expressions
  | Sub of expressions * expressions
  | Mult of expressions * expressions
  | Div of expressions * expressions
  | PostInc of expressions
  | PostDec of expressions
  | PrefInc of expressions
  | PrefDec of expressions
  | Mod of expressions * expressions
  | And of expressions * expressions
  | Or of expressions * expressions
  | Not of expressions
  | Equal of expressions * expressions
  | NotEqual of expressions * expressions
  | Less of expressions * expressions
  | More of expressions * expressions
  | LessOrEqual of expressions * expressions
  | MoreOrEqual of expressions * expressions
  | This
  | Base
  | Null
  | Value of values
  | Identifier of string
  | ClassCreation of names * expressions list
  | CallMethod of expressions * expressions list
  | AccessByPoint of expressions * expressions
  | Assign of expressions * expressions
  | Cast of types * expressions
[@@deriving show { with_path = false }]

and statements =
  | Expression of expressions
  | StatementBlock of statements list
  | If of expressions * statements * statements option
  | While of expressions * statements
  | For of
      statements option * expressions option * expressions list * statements
  | Break
  | Continue
  | Return of expressions option
  | VariableDecl of modifiers option * types * (names * expressions option) list
  | Print of expressions
[@@deriving show { with_path = false }]

and fields =
  | Field of types * (names * expressions option) list
  | Method of
      types (* type *)
      * names (* method name *)
      * names option (* parent class name *)
      * (types * names) list (* args *)
      * statements option (* body *)
  | Constructor of
      names * (types * names) list * expressions option * statements
[@@deriving show { with_path = false }]

and objects =
  | Class of
      modifiers list * names * names list * (modifiers list * fields) list
  | Interface of modifiers * names * names list * (modifiers list * fields) list
[@@deriving show { with_path = false }]
