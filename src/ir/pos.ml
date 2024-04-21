open Support.Pervasive

(** ポインタ移動コードを生成するためのテープ位置 (コード生成に利用) *)
type t =
  | Cell of int
  | Index of index
and index = {
  offset_of_head_in_parent: int;
  offset_of_index_in_array: int;
  size_of_members: int;
  child_pos: t;
}

let init = Cell 0

let shift_root n = function
  | Cell offset -> Cell (offset + n)
  | Index idx -> Index { idx with offset_of_head_in_parent=idx.offset_of_head_in_parent + n }

type ifable = {
  pos_cond: t;
  pos_else: t;
  pos_prev_endif: t;
  pos_endif: t;
  cond_to_else: int;
}
type selected =
  | SCell of t
  | SIfable of ifable

let selected_map f = function
  | SCell p -> SCell (f p)
  | SIfable ifable ->
      SIfable {
        pos_cond = f ifable.pos_cond;
        pos_else = f ifable.pos_else;
        pos_prev_endif = f ifable.pos_prev_endif;
        pos_endif = f ifable.pos_endif;
        cond_to_else = ifable.cond_to_else;
      }

let rec from_sel layout = function
  | Sel.Member m -> begin
      match Layout.lookup layout m with
      | Layout.Cell { offset; _ } -> SCell (Cell offset)
      | Ifable { offset_of_cond; cond_to_else; else_to_endif } ->
          SIfable {
            pos_cond = Cell offset_of_cond;
            pos_else = Cell (offset_of_cond + cond_to_else);
            pos_prev_endif = Cell (offset_of_cond + else_to_endif);
            pos_endif = Cell (offset_of_cond + cond_to_else + else_to_endif);
            cond_to_else
          }
      | Array _ -> assert false
    end
  | Array { name; index_opt=None; offset; member } -> begin
      match Layout.lookup layout name with
      | Cell _ | Ifable _ -> assert false
      | Layout.Array { offset_of_body; size_of_members; members; _ } ->
          from_sel members member
          |> selected_map (shift_root (offset_of_body + size_of_members*offset))
    end
  | Array { name; index_opt=Some index; offset; member } -> begin
      match Layout.lookup layout name with
      | Cell _ | Ifable _ -> assert false
      | Layout.Array { offset_of_body; size_of_members; members; _ } ->
          let offset_of_index_in_array =
            match Layout.lookup members index with
            | Cell { offset; _ } -> offset
            | Ifable _ | Array _ -> assert false
          in
          from_sel members member
          |> selected_map
              (fun pos ->
                Index {
                  offset_of_head_in_parent = offset_of_body +offset_of_index_in_array -size_of_members;
                  offset_of_index_in_array;
                  size_of_members;
                  child_pos = shift_root (size_of_members*offset) pos
                }
              )
    end

let from_sel_of_cell layout sel =
  let selected = from_sel layout sel in
  match selected with
  | SCell p -> p
  | SIfable { pos_cond; _ } -> pos_cond

let from_sel_of_ifable layout sel =
  let selected = from_sel layout sel in
  match selected with
  | SCell _ -> assert false
  | SIfable ifable -> ifable

(** 配列のインデックスを遡ってヘッダまで戻るコードを生成する *)
let rec gen_bf_move_root { offset_of_index_in_array; size_of_members; child_pos; _ }: Bf.Code.t =
  let origin_offset_in_array = match child_pos with
    | Cell offset -> offset
    | Index child -> child.offset_of_head_in_parent
  in
  let code_origin_to_head = [
    Bf.Code.Shift (offset_of_index_in_array -origin_offset_in_array -size_of_members);
    Bf.Code.Loop [
      Bf.Code.Shift (-size_of_members)
    ]
  ] in
  match child_pos with
  | Cell _ -> code_origin_to_head
  | Index child -> gen_bf_move_root child @ code_origin_to_head

(** [gen_bf_move origin dest]  [origin]から[dest]に移動するコードを生成する *)
let rec gen_bf_move origin dest: Bf.Code.t =
  match origin, dest with
  | Index origin, Index dest
    when
      (* XXX: もし配列がheadを共有する実装になると再びバグるので要修正 *)
      origin.offset_of_head_in_parent = dest.offset_of_head_in_parent &&
      origin.offset_of_index_in_array = dest.offset_of_index_in_array
    ->
      gen_bf_move origin.child_pos dest.child_pos
  | Index origin, dest ->
      gen_bf_move_root origin
        @ gen_bf_move (Cell origin.offset_of_head_in_parent) dest
  | Cell origin, Index dest ->
      let code = [
        Bf.Code.Shift (dest.offset_of_head_in_parent +dest.size_of_members -origin);
        Bf.Code.Loop [
          Bf.Code.Shift dest.size_of_members
        ]
      ] in
      code @ gen_bf_move (Cell dest.offset_of_index_in_array) (dest.child_pos)
  | Cell origin, Cell dest -> [ Bf.Code.Shift (dest -origin) ]