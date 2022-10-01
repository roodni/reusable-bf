(** 変数名とテープ位置の確保の仕方(mtype)の対応 *)
type t = (Id.t, mtype) Hashtbl.t
and mtype =
  | Cell of { mutable ifable: bool; mergeable: bool }
  | Array of { length: int; members: t }
  | Index
type main = { finite: t; unlimited: t }

let uarray_id = Id.gen_special "uarray"

let empty (): t = Hashtbl.create 30
let lookup (field: t) id = Hashtbl.find field id
let extend (field: t) id mtype = Hashtbl.replace field id mtype

let fold f (field: t) init =
  Hashtbl.to_seq field |>
    List.of_seq |>
    List.sort
      (fun (id1, _) (id2, _) ->
        Int.compare (Id.to_definition_order id1) (Id.to_definition_order id2)) |>
    List.fold_left
      (fun accu (id, mtype) -> f id mtype accu)
      init

let empty_main (): main = { finite=empty (); unlimited=empty () }
let lookup_main (main: main) id =
  if id = uarray_id
    then Array { members=main.unlimited; length=1; }
    else lookup main.finite id