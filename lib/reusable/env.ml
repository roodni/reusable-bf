module Make(V: Map.OrderedType) = struct
  module M = Map.Make(V)

  type key = V.t
  (* 内側から見える環境, エクスポートする環境 *)
  type 'a t = 'a M.t

  let empty = M.empty
  let extend key value env =
    M.add key value env
  let lookup key env = M.find_opt key env

  let merge src dest =
    M.union
      (fun _key src _dest -> Some src)
      src dest
end