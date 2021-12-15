module StringMap = Map.Make(String)

type t = Syntax.poly_t StringMap.t

let empty : t = StringMap.empty

let singleton k v : t = StringMap.singleton k v

let extend k v e : t = StringMap.add k v e

let remove k e : t = StringMap.remove k e

let find_opt d k e =
  match StringMap.find_opt k e with
  | Some t -> t
  | None -> d

let map f s : t = StringMap.map f s

let to_seq = StringMap.to_seq
