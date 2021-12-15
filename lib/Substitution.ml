module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

open Syntax

type t = mono_t StringMap.t

let empty : t = StringMap.empty

let singleton (k : string) (v : mono_t) : t =
  StringMap.singleton k v

let extend (s : t) (k : string) (v : mono_t) : t =
  StringMap.add k v s

module type Substitutable = sig
  type s

  val apply_subst : t -> s -> s

  val free_vars : s -> StringSet.t
end

module MkSubstitutable (S : Substitutable) = struct
  type s = S.s

  let apply_subst = S.apply_subst

  let free_vars = S.free_vars

  let occurs_check (x : string) (t : S.s) : bool =
    StringSet.mem x (S.free_vars t)
end

module Mono = MkSubstitutable(struct
  type s = mono_t

  let rec apply_subst (s : t) : s -> s = function
    | MonoUnit -> MonoUnit
    | MonoVar v ->
       let result = match StringMap.find_opt v s with
         | Some t -> t
         | None -> MonoVar v
       in result
    | MonoFun (x, y) ->
       MonoFun (apply_subst s x, apply_subst s y)

  let rec free_vars : s -> StringSet.t = function
    | MonoUnit -> StringSet.empty
    | MonoVar v -> StringSet.singleton v
    | MonoFun (x, y) ->
       StringSet.union (free_vars x) (free_vars y)
end)

module Poly = MkSubstitutable(struct
  type s = poly_t

  let apply_subst (s : t) (t : s) : s =
    let rec aux qs = function
      | PolyMono m ->
         let s = List.fold_right StringMap.remove qs s in
         let m = PolyMono (Mono.apply_subst s m) in
         List.fold_right (fun a b -> PolyQuan (a, b)) qs m
      | PolyQuan (q, p) ->
         aux (q :: qs) p
    in aux [] t

  let free_vars (t : s) : StringSet.t =
    let rec aux qs = function
      | PolyMono m ->
         StringSet.diff (Mono.free_vars m) qs
      | PolyQuan (q, p) ->
         aux (StringSet.add q qs) p
    in
    aux StringSet.empty t
end)

module Env = MkSubstitutable(struct
  type s = Environment.t

  let apply_subst (s : t) (e : s) : s =
    Environment.map (Poly.apply_subst s) e

  let free_vars (e : s) : StringSet.t =
    StringSet.of_seq (Seq.map (fun (k, _) -> k) (Environment.to_seq e))
end)

let compose (f : t) (g : t) : t =
  let unionWithConst = StringMap.union (fun _ a _ -> Some a) in
  unionWithConst (StringMap.map (Mono.apply_subst f) g) f
