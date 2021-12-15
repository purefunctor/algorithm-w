module StringSet : Set.S with type elt = string

type t

val empty : t

val singleton : string -> Syntax.mono_t -> t

val extend : t -> string -> Syntax.mono_t -> t

val compose : t -> t -> t

module Mono : sig
  type s = Syntax.mono_t

  val apply_subst : t -> s -> s

  val free_vars : s -> StringSet.t

  val occurs_check : string -> s -> bool
end

module Poly : sig
  type s = Syntax.poly_t

  val apply_subst : t -> s -> s

  val free_vars : s -> StringSet.t

  val occurs_check : string -> s -> bool
end

module Env : sig
  type s = Environment.t

  val apply_subst : t -> s -> s

  val free_vars : s -> StringSet.t

  val occurs_check : string -> s -> bool
end
