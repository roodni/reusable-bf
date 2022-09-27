let (--) a b =
  let len = b - a + 1 in
  if len < 0 then []
  else List.init len (fun i -> a + i)

module String = struct
  include String
  let repeat s n =
    let buf = Buffer.create (String.length s * n) in
    (1 -- n) |> List.iter (fun _ -> Buffer.add_string buf s);
    Buffer.contents buf
end

module Hashtbl = struct
  include Hashtbl
  let find_default tbl key default =
    find_opt tbl key |> Option.value ~default
  let add_assign_int tbl key n =
    replace tbl key (find_default tbl key 0 + n)
end