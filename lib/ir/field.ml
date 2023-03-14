(** 変数名とテープ位置の確保の仕方(mtype)の対応 *)
type t = (Id.t, mtype) Hashtbl.t
and mtype =
  | Cell of { mutable ifable: bool; sticky: bool; index: Sel.index option; }
      (* sticky: finite直下のセル・または配列メンバの一時セル *)
  | Array of { length: int; members: t }
  | Index
type main = { finite: t; unlimited: t }

let uarray_id = Id.gen_special "UA"

let empty (): t = Hashtbl.create 30
let lookup (field: t) id = Hashtbl.find field id
let extend (field: t) id mtype = Hashtbl.replace field id mtype

let fold f (field: t) init =
  Hashtbl.to_seq field
  |> List.of_seq
  |> List.sort (fun (id1, _) (id2, _) -> Id.compare id1 id2)
  |> List.fold_left
    (fun accu (id, mtype) -> f id mtype accu)
    init

let empty_main (): main = { finite=empty (); unlimited=empty () }
let lookup_main (main: main) id =
  if id = uarray_id
    then Array { members=main.unlimited; length=1; }
    else lookup main.finite id

(** セレクタの指すmtypeを取得する *)
let find_by_sel fmain sel =
  let rec find_by_sel field = function
    | Sel.Member id ->
        if fmain.finite == field
          then lookup_main fmain id
          else lookup field id
    | Array { name; member; _ } -> begin
        let mtype =
          if fmain.finite == field
            then lookup_main fmain name
            else lookup field name
        in
        match mtype with
        | Array { members; _ } -> find_by_sel members member
        | Cell _ | Index -> assert false
      end
  in
  find_by_sel fmain.finite sel