open Printf

(** Idを使ってテープ位置を指すセレクタ *)
type t =
  | Member of Id.t
  | Array of { name: Id.t; index_opt: Id.t option; offset: int; member: t }

let head_id = function
  | Member id -> id
  | Array { name; _ } -> name

let rec concat_member_to_index_tail (arr_sel, idx_id) mem_id offset =
  match arr_sel with
  | Member id -> Array { name=id; index_opt=Some idx_id; member=Member mem_id; offset }
  | Array mid_array ->
      Array { mid_array with member=concat_member_to_index_tail (mid_array.member, idx_id) mem_id offset }

(** セレクタの指すテープ位置のmtypeを取得する *)
let find_field (fmain: Field.main) (sel: t) =
  let rec find_field field = function
    | Member id ->
        if fmain.finite == field
          then Field.lookup_main fmain id
          else Field.lookup field id
    | Array { name; member; _ } ->
        let mtype =
          if fmain.finite == field
            then Field.lookup_main fmain name
            else Field.lookup field name
        in begin
          match mtype with
          | Field.Array { members; _ } -> find_field members member
          | _ -> assert false
        end
  in
  find_field fmain.finite sel

let rec pretty = function
  | Member id -> Id.to_string id
  | Array { name; index_opt=None; offset; member } ->
      sprintf "%s:(%d)%s" (Id.to_string name) offset (pretty member)
  | Array { name; index_opt=Some index; offset; member } ->
      sprintf "%s@%s:(%d)%s" (Id.to_string name) (Id.to_string index) offset (pretty member)