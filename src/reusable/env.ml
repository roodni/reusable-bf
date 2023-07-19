module Make(V: Map.OrderedType) = struct
  module M = Map.Make(V)

  type key = V.t
  type 'a t = 'a M.t

  let empty = M.empty
  let extend key value env =
    M.add key value env
  let lookup key env = M.find_opt key env
  let to_seq env = M.to_seq env

  let merge src dest =
    M.union
      (fun _key src _dest -> Some src)
      src dest
end