open Printf

module Var: sig
  type t
  val gen: unit -> t
  val gen_named: string -> t
  val to_string: t -> string
  val infarray: t
end = struct
  type t = int
  let num = ref 0
  let nametable = Hashtbl.create 30
  let gen (): t =
    incr num;
    !num
  let gen_named (name: string): t =
    let id = gen () in
    Hashtbl.add nametable id name;
    id
  let to_string (t: t) =
    let name = match Hashtbl.find_opt nametable t with
      | None -> ""
      | Some name -> name
    in
    sprintf "%s#%d" name t
  let infarray = gen_named "<infarray>"
end

(** 変数名とテープ位置の確保の仕方(mtype)の対応 *)
module Field = struct
  type t = (Var.t, mtype) Hashtbl.t
  and mtype =
    | Cell of { mutable ifable: bool }
    | Array of { length: int; members: t }
    | Index
  type main = { finite: t; infarray: t }

  (** インデックスとそれ以外を分離する *)
  let partition_index (field: t) =
    let indexes, other =
      field |> Hashtbl.to_seq |> List.of_seq
        |> List.partition (fun (_, mtype) -> mtype = Index)
    in
    (indexes |> List.to_seq |> Hashtbl.of_seq,
      other |> List.to_seq |> Hashtbl.of_seq)

  let empty (): t = Hashtbl.create 30
  let lookup (field: t) var = Hashtbl.find field var
  let extend (field: t) var mtype = Hashtbl.replace field var mtype

  let empty_main (): main = { finite=empty (); infarray=empty () }
  let lookup_main (main: main) var =
    if var = Var.infarray
      then Array { members=main.infarray; length=(-1); }
      else lookup main.finite var
end

(** 変数名を使ってテープ位置を指すセレクタ *)
module Sel = struct
  type t =
    | Member of Var.t
    | Array of { name: Var.t; index_opt: Var.t option; offset: int; member: t }

  (** 配列へのセレクタとインデックス名から、自分自身を経由して自分にアクセスするインデックスのセレクタを取得する *)
  let rec index_on_itself array index offset =
    match array with
    | Member v -> Array { name=v; index_opt=Some index; offset; member=Member index }
    | Array array_mid -> Array { array_mid with member=index_on_itself array_mid.member index offset }

  (** セレクタの指すテープ位置のmtypeを取得する *)
  let find_field (fmain: Field.main) (sel: t) =
    let rec find_field field = function
      | Member v ->
          if fmain.finite == field
            then Field.lookup_main fmain v
            else Field.lookup field v
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
    | Member v -> Var.to_string v
    | Array { name; index_opt=None; offset; member } ->
        sprintf "%s:(%d)%s" (Var.to_string name) offset (pretty member)
    | Array { name; index_opt=Some index; offset; member } ->
        sprintf "%s@%s:(%d)%s" (Var.to_string name) (Var.to_string index) offset (pretty member)
end

(** 中間言語のコード *)
module Code = struct
  type t = cmd list
  and cmd =
    | Add of int * Sel.t
    | Put of Sel.t
    | Get of Sel.t
    | Loop of Sel.t * t
    | LoopPtr of (Sel.t * Var.t * t)
    | Shift of int * Sel.t * Var.t
    | If of Sel.t * t * t
end

(** 変数名と確保されたテープ位置の対応 *)
module Layout = struct
  type t = (Var.t * loc) list
  and loc =
    | Cell of int
    | Ifable of { offset_of_cond: int; cond_to_else: int; else_to_endif: int; }
    | Array of {
        offset_of_body: int; (* メンバを格納するエリアの左端 (さらに左側にはインデックスだけを含むヘッダがある) *)
        size_of_members: int;
        members: t
      }

  (** 定義された変数をセルに割り当てる *)
  let from_field (field: Field.main): t =
    let Field.{ finite; infarray } = field in
    let rec allocate (field: Field.t) (ofs_available: int): t * int =
      (* 左から詰める *)
      Hashtbl.fold (fun var kind (layout, ofs_available) ->
        let loc, ofs_available =
          match kind with
          | Field.Cell { ifable=false } -> (Cell ofs_available, ofs_available + 1)
          | Cell { ifable=true } ->
              (Ifable { offset_of_cond=ofs_available; cond_to_else=1; else_to_endif=1 }, ofs_available + 3)
          | Index -> (Cell ofs_available, ofs_available + 1)
          | Array { members; length } ->
              let indexes, members = Field.partition_index members in
              (* メンバを左から詰める *)
              let layout, ofs_index_start = allocate members 0 in
              (* ポインタを詰める *)
              let layout_indexes, size_of_members = allocate indexes ofs_index_start in
              let members = layout @ layout_indexes in
              let offset_of_body = ofs_available + size_of_members - ofs_index_start in
              ( Array { offset_of_body; size_of_members; members },
                offset_of_body + size_of_members*length )
        in
        ((var, loc) :: layout, ofs_available)
      ) field ([], ofs_available)
    in
    let layout, ofs_infarray_start = allocate finite 0 in
    let infarray = (Var.infarray, Field.Array { members=infarray; length=1 })
      |> Seq.return |> Hashtbl.of_seq
    in
    let layout_infarray, _ = allocate infarray ofs_infarray_start in
    layout @ layout_infarray

  let lookup (layout: t) v = List.assoc v layout
end

(** ポインタ移動コードを生成するためのテープ位置 (コード生成に利用) *)
module Pos = struct
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
        | Layout.Cell offset -> SCell (Cell offset)
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
        | Layout.Array { offset_of_body; size_of_members; members } ->
            from_sel members member
            |> selected_map (shift_root (offset_of_body + size_of_members*offset))
      end
    | Array { name; index_opt=Some index; offset; member } -> begin
        match Layout.lookup layout name with
        | Cell _ | Ifable _ -> assert false
        | Layout.Array { offset_of_body; size_of_members; members } ->
            let offset_of_index_in_array =
              match Layout.lookup members index with
              | Cell offset -> offset
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

  let from_sel_to_cell layout sel =
    let selected = from_sel layout sel in
    match selected with
    | SCell p -> p
    | SIfable { pos_cond; _ } -> pos_cond

  let from_sel_to_ifable layout sel =
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
      when origin.offset_of_index_in_array = dest.offset_of_index_in_array ->
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
end

let gen_bf (layout: Layout.t) (code: Code.t): Bf.Code.t =
  let rec gen_bf (pos_init: Pos.t) (code: Code.t) =
    let pos, bf_cmd_list_list = List.fold_left_map (fun pos cmd ->
        match cmd with
        | Code.Add (0, _) -> (pos, [])
        | Add (n, sel) ->
            let pos_dest = Pos.from_sel_to_cell layout sel in
            (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Add n ])
        | Put sel ->
            let pos_dest = Pos.from_sel_to_cell layout sel in
            (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Put ])
        | Get sel ->
            let pos_dest = Pos.from_sel_to_cell layout sel in
            (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Get ])
        | Loop (sel, code) ->
            let pos_cond = Pos.from_sel_to_cell layout sel in
            let bf_move1 = Pos.gen_bf_move pos pos_cond in
            let pos, bf_loop = gen_bf pos_cond code in
            let bf_move2 = Pos.gen_bf_move pos pos_cond in
            (pos_cond, bf_move1 @ [ Bf.Code.Loop (bf_loop @ bf_move2) ])
        | LoopPtr (array, index, code) ->
            let sel_cond = Sel.index_on_itself array index (-1) in
            gen_bf pos [ Loop (sel_cond, code) ]
        | Shift (n, array, idx) -> begin
            let pos_ptr = Sel.index_on_itself array idx 0 |> Pos.from_sel_to_cell layout in
            let pos_ptr_prev = Sel.index_on_itself array idx (-1) |> Pos.from_sel_to_cell layout in
            match n with
            | 0 -> (pos, [])
            | 1 ->
                let bf_move = Pos.gen_bf_move pos pos_ptr in
                let bf = bf_move @ [ Bf.Code.Add 1 ] in
                (pos_ptr_prev, bf)
            | -1 ->
                let bf_move = Pos.gen_bf_move pos pos_ptr_prev in
                let bf = bf_move @ [ Bf.Code.Add (-1) ] in
                (pos_ptr, bf)
            | _ -> failwith "not implemented"
          end
        | If (cond, code_then, code_else) ->
            let ifable = Pos.from_sel_to_ifable layout cond in
            let pos_then_end, bf_then = gen_bf ifable.pos_else code_then in
            let pos_else_end, bf_else = gen_bf ifable.pos_else code_else in
            let bf =
              Pos.gen_bf_move pos ifable.pos_else @
              [ Bf.Code.Add 1;
                Bf.Code.Shift (-ifable.cond_to_else);
                Bf.Code.Loop (
                  [ Bf.Code.Shift ifable.cond_to_else;
                    Bf.Code.Add (-1);
                  ] @
                  bf_then @
                  Pos.gen_bf_move pos_then_end ifable.pos_prev_endif
                );
                Bf.Code.Shift ifable.cond_to_else;
                Bf.Code.Loop (
                  [ Bf.Code.Add (-1) ] @
                  bf_else @
                  Pos.gen_bf_move pos_else_end ifable.pos_endif
                );
              ]
            in
            (ifable.pos_endif, bf)
      ) pos_init code
    in
    (pos, List.flatten bf_cmd_list_list)
  in
  let _, bf = gen_bf Pos.init code in
  bf
