open Substitution
open Syntax

let fresh =
  let i = ref 0 in
  fun () ->
  let i' = string_of_int !i in
  incr i;
  String.cat "t" i'

let instantiate =
  let rec aux s = function
    | PolyMono m ->
       Mono.apply_subst s m
    | PolyQuan (q, p) ->
       let q' = MonoVar (fresh ()) in
       aux (Substitution.extend s q q') p
  in aux Substitution.empty

let generalize e t =
  let list_of_seq s = StringSet.to_seq s |> List.of_seq in
  let vs = StringSet.diff (Mono.free_vars t) (Env.free_vars e) in
  List.fold_right (fun v t -> PolyQuan (v, t)) (list_of_seq vs) (PolyMono t)

let rec infer environment = function
  | ExprUnit ->
     (MonoUnit, Constraints.empty)
  | ExprVar v ->
     let d = PolyMono (MonoVar v) in
     let v_t = Environment.find_opt d v environment in
     (instantiate v_t, Constraints.empty)
  | ExprLam (v, e) ->
     let unsolved = MonoVar (fresh ()) in
     let environment =
       Environment.(remove v environment |> extend v (PolyMono unsolved))
     in
     let (t, constraints) = infer environment e in
     (MonoFun (unsolved, t), constraints)
  | ExprApp (f, x) ->
     let (f_t, f_c) = infer environment f in
     let (x_t, x_c) = infer environment x in
     let unsolved = MonoVar (fresh ()) in
     let constraints = [f_t, MonoFun (x_t, unsolved)] in
     (unsolved, List.concat [f_c;x_c;constraints])
  | ExprLet (x, v, e) ->
     let (v_t, v_c) = infer environment v in
     let v_s = Constraints.solve v_c in
     let v_t =
       generalize
         (Env.apply_subst v_s environment)
         (Mono.apply_subst v_s v_t)
     in
     let environment =
       Environment.(
         remove x environment |> Env.apply_subst v_s |> extend x v_t
       )
     in
     let (e_t, e_c) = infer environment e in
     (e_t, List.concat [v_c;e_c])

let inferExpression e =
  let (t, c) = infer Environment.empty e in
  let s = Constraints.solve c in
  Mono.apply_subst s t |> generalize Environment.empty
