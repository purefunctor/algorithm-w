val instantiate : Syntax.poly_t -> Syntax.mono_t

val generalize : Environment.t -> Syntax.mono_t -> Syntax.poly_t

val infer : Environment.t -> Syntax.expr_t -> (Syntax.mono_t * Constraints.t)

val inferExpression : Syntax.expr_t -> Syntax.poly_t
