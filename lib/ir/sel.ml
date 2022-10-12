open Printf

(** Idを使ってテープ位置を指すセレクタ *)
type t =
  | Member of Id.t
  | Array of { name: Id.t; index_opt: Id.t option; offset: int; member: t }
type index = t * Id.t

let head_id = function
  | Member id -> id
  | Array { name; _ } -> name
let rec last_id = function
  | Member id -> id
  | Array { member; _ } -> last_id member

let rec convert_id (f: Id.t -> Id.t) = function
  | Member id -> Member (f id)
  | Array { name; index_opt; offset; member } ->
      Array {
        name = f name;
        index_opt = Option.map f index_opt;
        offset;
        member = convert_id f member
      }

let concat_member_to_tail
    (arr_sel: t) (idx_id_opt: Id.t option) (member: t) offset =
  let rec concat_member_to_tail = function
    | Member name -> Array { name; index_opt=idx_id_opt; member; offset }
    | Array mid_arr ->
        Array {
          mid_arr with
          member = concat_member_to_tail mid_arr.member
        }
  in
  concat_member_to_tail arr_sel

(** セレクタがインデックス[index]を経由するものかどうか *)
let is_via_index (index: index) sel =
  let _, idx_id = index in (* よくわからないので保守的にやる *)
  let rec is_via_index = function
    | Member _ -> false
    | Array { index_opt=None; member; _ } -> is_via_index member
    | Array { index_opt=Some idx_id_of_sel; member; _ } ->
        if idx_id = idx_id_of_sel then true
          else is_via_index member
  in
  is_via_index sel

let concat_member_to_index_tail (arr_sel, idx_id: index) mem_id offset =
  concat_member_to_tail arr_sel (Some idx_id) (Member mem_id) offset

let concat_member_to_index_opt_tail index_opt mem_id offset =
  match index_opt, offset with
  | None, 0 -> Member mem_id
  | None, _ -> assert false
  | Some index, _ -> concat_member_to_index_tail index mem_id offset
;;

(** セレクタの指すテープ位置のmtypeを取得する *)
let find_mtype (fmain: Field.main) (sel: t) =
  let rec find_mtype field = function
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
          | Field.Array { members; _ } -> find_mtype members member
          | _ -> assert false
        end
  in
  find_mtype fmain.finite sel

let rec to_string = function
  | Member id -> Id.numbered_name id
  | Array { name; index_opt=None; offset; member } ->
      sprintf "%s:(%d)%s" (Id.simple_name name) offset (to_string member)
  | Array { name; index_opt=Some index; offset; member } ->
      if offset = 0 then
        sprintf "%s@%s:%s" (Id.simple_name name) (Id.simple_name index) (to_string member)
      else
        sprintf "%s@%s:(%d)%s" (Id.simple_name name) (Id.simple_name index) offset (to_string member)