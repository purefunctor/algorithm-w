open Substitution
open Syntax

(* TODO: [failwith/raise] unify *)
let rec unify (a : mono_t) (b : mono_t) : Substitution.t =
  match (a, b) with
  | MonoUnit, MonoUnit ->
     Substitution.empty
  | MonoVar a, MonoVar b when a = b ->
     Substitution.empty
  | MonoVar a, b | b, MonoVar a ->
     if Mono.occurs_check a b then
       failwith "infinite type"
     else
       Substitution.singleton a b
  | MonoFun (a, b), MonoFun (a', b') ->
     let f = unify a a' in
     let g = unify (Mono.apply_subst f b) (Mono.apply_subst f b') in
     Substitution.compose g f
  | _ ->
     failwith "does not unify"
