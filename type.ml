type t =
  | Int
  | Char
  | Unit
  | Bool
  | String
  | Tuple of t list
  | Custom of string
  | Var of string
  | CtorApp of t * string
  | Args of t list
  | Func of t * t
