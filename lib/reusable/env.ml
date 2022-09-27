module Make(V: Map.OrderedType) = struct
  module M = Map.Make(V)

  type key = V.t
  (* 内側から見える環境, エクスポートする環境 *)
  type 'a t = 'a M.t * 'a M.t

  let empty = (M.empty, M.empty)
  let extend ?(export=false) key value (map_in, map_ex) =
    let map_in = M.add key value map_in in
    let map_ex =
      if export then M.add key value map_ex else map_ex
    in
    (map_in, map_ex)
  let lookup key (map_in, _) = M.find_opt key map_in
  let export (_, map_ex) = (map_ex, map_ex)

  let import (_, src_ex) (dest_in, dest_ex) =
    let dest_in =
      M.union
        (fun _key src _dest -> Some src)
        src_ex dest_in
    in
    (dest_in, dest_ex)
end