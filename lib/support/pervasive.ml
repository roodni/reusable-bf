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


type 'a llist = 'a list
let llist l = l
let (~~) = llist
let lcons = List.cons
let lnil = []
let (@+) a b =
  List.rev_append (List.rev a) b

module LList = struct
  type 'a t = 'a llist

  let to_list_danger l = l

  let length = List.length
  let rev = List.rev
  let concat ll = List.concat_map Fun.id ll

  let iter = List.iter
  let iteri = List.iteri
  let map f l = List.rev_map f l |> List.rev
  let filter_map = List.filter_map
  let fold_left_map = List.fold_left_map
  let fold_left = List.fold_left

  let find_map = List.find_map
  let filter = List.filter

  let assoc = List.assoc
  let assoc_opt = List.assoc_opt

  let sort = List.sort

  let to_seq = List.to_seq
  let of_seq = List.of_seq
end