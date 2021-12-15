open Substitution
open Syntax
open Unification

type t = (mono_t * mono_t) list

let empty : t = []

let apply (s : Substitution.t) : t -> t =
  List.map (fun (a, b) -> (Mono.apply_subst s a, Mono.apply_subst s b))

let solve (c : t) : Substitution.t =
  let rec aux s = function
    | [] -> s
    | (x, x') :: xs ->
       let s' = Substitution.compose (unify x x') s in
       aux s' (apply s' xs)
  in aux Substitution.empty c
