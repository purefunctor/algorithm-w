type t

val empty : t

val singleton : string -> Syntax.poly_t -> t

val extend : string -> Syntax.poly_t -> t -> t

val remove : string -> t -> t

val find_opt : Syntax.poly_t -> string -> t -> Syntax.poly_t

val map : (Syntax.poly_t -> Syntax.poly_t) -> t -> t

val to_seq : t -> (string * Syntax.poly_t) Seq.t
