type t = (Syntax.mono_t * Syntax.mono_t) list

val empty : t

val solve : t -> Substitution.t
