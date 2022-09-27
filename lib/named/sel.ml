open Printf

(** Idを使ってテープ位置を指すセレクタ *)
type t =
  | Member of Id.t
  | Array of { name: Id.t; index_opt: Id.t option; offset: int; member: t }

let head_id = function
  | Member id -> id
  | Array { name; _ } -> name

(** 配列へのセレクタとインデックス名から、自分自身を経由して自分にアクセスするインデックスのセレクタを取得する *)
let rec index_on_itself array index offset =
  match array with
  | Member id -> Array { name=id; index_opt=Some index; offset; member=Member index }
  | Array array_mid -> Array { array_mid with member=index_on_itself array_mid.member index offset }

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