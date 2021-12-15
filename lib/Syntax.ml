type expr_t =
  | ExprUnit
  | ExprVar of string
  | ExprApp of expr_t * expr_t
  | ExprLam of string * expr_t
  | ExprLet of string * expr_t * expr_t

type mono_t =
  | MonoUnit
  | MonoVar of string
  | MonoFun of mono_t * mono_t

type poly_t =
  | PolyMono of mono_t
  | PolyQuan of string * poly_t
