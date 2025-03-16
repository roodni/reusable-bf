module Make(V: Map.OrderedType) = struct
  module M = Map.Make(V)

  type key = V.t
  type 'a t = 'a ref M.t

  let empty = M.empty

  let extend key value env =
    M.add key (ref value) env

  (** let rec用 *)
  let extend_ref key rvalue env =
    M.add key rvalue env

  let lookup key env =
    M.find_opt key env
    |> Option.map (!)

  let to_seq env =
    M.to_seq env
    |> Seq.map (fun (k, v) -> k, !v)

  let merge src dest =
    M.union
      (fun _key src _dest -> Some src)
      src dest
end