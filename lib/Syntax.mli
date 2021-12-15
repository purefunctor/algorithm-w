(** The type of expressions *)
type expr_t =
  | ExprUnit
  | ExprVar of string
  | ExprApp of expr_t * expr_t
  | ExprLam of string * expr_t
  | ExprLet of string * expr_t * expr_t

(** The type of monomorphic types *)
type mono_t =
  | MonoUnit
  | MonoVar of string
  | MonoFun of mono_t * mono_t

(** The type of polymorphic types *)
type poly_t =
  | PolyMono of mono_t
  | PolyQuan of string * poly_t
