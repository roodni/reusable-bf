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
  and loc = {
    offset: int;
    kind: kind
  }
  and kind =
    | Ptr
    | Cell
    | CellIfable
    | Lst of lst
  and lst = {
    mem: t;
    header_start: int;
    elm_size: int;
  }

  (** 定義された変数をセルに割り当てる *)
  let from_field (field: Field.main): t =
    let Field.{ finite; infarray } = field in
    let rec allocate (field: Field.t) (ofs_available: int): t * int =
      (* 左から詰める *)
      Hashtbl.fold (fun var kind (layout, ofs_available) ->
        let loc, ofs_available =
          match kind with
          | Field.Cell { ifable=false } -> ({ offset = ofs_available; kind = Cell }, ofs_available + 1)
          | Cell { ifable=true } -> ({ offset = ofs_available; kind = CellIfable }, ofs_available + 3)
          | Index -> ({ offset = ofs_available; kind = Ptr }, ofs_available + 1)
          | Array { members; length } ->
              let indexes, members = Field.partition_index members in
              (* メンバを左から詰める *)
              let layout, ofs_index_start = allocate members 0 in
              let header_start =  -ofs_index_start in
              (* ポインタを詰める *)
              let layout_indexes, elm_size = allocate indexes ofs_index_start in
              let layout = layout @ layout_indexes in
              let arr = { mem=layout; header_start; elm_size } in
              ( { offset = ofs_available; kind = Lst arr },
                ofs_available + elm_size*(length + 1) + header_start )
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

  let loc_of_var (layout: t) (v: Var.t) = List.assoc_opt v layout
end

(** ポインタ移動コードを生成するためのテープ位置 (コード生成に利用) *)
module Pos = struct
  type t =
    | Cell of int
    | Ptr of ptr
  and ptr = {
    offset_of_head: int;
    offset_in_lst: int;
    size: int;
    child_pos: t;
  }

  let init = Cell 0

  let shift n = function
    | Cell offset -> Cell (offset + n)
    | Ptr ({ offset_of_head; _ } as ptr) -> Ptr { ptr with offset_of_head = offset_of_head + n; }

  let rec shift_last n pos =
    match pos with
    | Cell offset -> Cell (offset + n)
    | Ptr ({ child_pos; _ } as ptr) -> Ptr { ptr with child_pos = shift_last n child_pos }

  let from_sel (layout: Layout.t) (sel: Sel.t) =
    let raise_not_found () =
      failwith @@ sprintf "not found: %s" (Sel.pretty sel)
    in
    let rec of_sel layout sel =
      match sel with
      | Sel.Member m -> begin
          match Layout.loc_of_var layout m with
          | None -> raise_not_found ()
          | Some loc -> Cell loc.offset
        end
      | Array { name=lst; index_opt=None; offset=i; member=sel } ->
          let loc = match Layout.loc_of_var layout lst with
            | None -> raise_not_found ()
            | Some loc -> loc
          in
          let mem, header_start, elm_size =
            match loc.kind with
            | Layout.Lst { mem; header_start; elm_size; _ } ->
                (mem, header_start, elm_size)
            | _ -> assert false
          in
          let offset = loc.offset + header_start + (1 + i) * elm_size in
          of_sel mem sel |> shift offset
      | Array { name=lst; index_opt=Some ptr; offset=i; member=sel } ->
          let loc, lst =
            match Layout.loc_of_var layout lst with
            | None -> raise_not_found ()
            | Some ({ kind = Layout.Lst lst; _ } as loc) -> (loc, lst)
            | Some _ -> assert false
          in
          let ptr_offset_in_lst =
            match Layout.loc_of_var lst.mem ptr with
            | None -> raise_not_found ()
            | Some { offset; kind = Ptr } -> offset
            | Some _ -> assert false
          in
          let index = i * lst.elm_size in
          let child_pos = of_sel lst.mem sel |> shift index in
          Ptr {
            offset_of_head = loc.offset + lst.header_start + ptr_offset_in_lst;
            offset_in_lst = ptr_offset_in_lst;
            size = lst.elm_size;
            child_pos;
          }
    in
    of_sel layout sel

  (** リストのポインタを遡って根本のoffset_of_headまで戻るコードを生成する *)
  let rec gen_bf_move_root { offset_in_lst; size; child_pos; _ }: Bf.Code.t =
    let origin_offset = match child_pos with
      | Cell offset -> offset
      | Ptr { offset_of_head; _ } -> offset_of_head
    in
    let code = [
      Bf.Code.Shift (offset_in_lst - origin_offset - size);
      Bf.Code.Loop [
        Bf.Code.Shift (-size)
      ]
    ] in
    match child_pos with
    | Cell _ -> code
    | Ptr ptr -> gen_bf_move_root ptr @ code

  (** [gen_bf_move origin dest]  [origin]から[dest]に移動するコードを生成する *)
  let rec gen_bf_move (origin: t) (dest: t): Bf.Code.t =
    match origin, dest with
    | Ptr origin, Ptr dest when origin.offset_of_head = dest.offset_of_head ->
        gen_bf_move origin.child_pos dest.child_pos
    | Ptr origin, dest ->
        gen_bf_move_root origin @ gen_bf_move (Cell origin.offset_of_head) dest
    | Cell origin, Ptr dest ->
        let code = [
          Bf.Code.Shift (dest.offset_of_head + dest.size - origin);
          Bf.Code.Loop [
            Bf.Code.Shift dest.size
          ]
        ] in
        code @ gen_bf_move (Cell dest.offset_in_lst) (dest.child_pos)
    | Cell origin, Cell dest -> [ Bf.Code.Shift (dest - origin) ]
end

let gen_bf (layout: Layout.t) (code: Code.t): Bf.Code.t =
  let rec gen_bf (pos_init: Pos.t) (code: Code.t) =
    let pos, bf_cmd_list_list = List.fold_left_map (fun pos cmd ->
        match cmd with
        | Code.Add (0, _) -> (pos, [])
        | Add (n, sel) ->
            let pos_dest = Pos.from_sel layout sel in
            (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Add n ])
        | Put sel ->
            let pos_dest = Pos.from_sel layout sel in
            (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Put ])
        | Get sel ->
            let pos_dest = Pos.from_sel layout sel in
            (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Get ])
        | Loop (sel, cmd_list) ->
            let pos_cond = Pos.from_sel layout sel in
            let code_move1 = Pos.gen_bf_move pos pos_cond in
            let pos, code_loop = gen_bf pos_cond cmd_list in
            let code_move2 = Pos.gen_bf_move pos pos_cond in
            (pos_cond, code_move1 @ [ Bf.Code.Loop (code_loop @ code_move2) ])
        | LoopPtr (lst, ptr, cmd_list) ->
            let sel_cond = Sel.index_on_itself lst ptr (-1) in
            gen_bf pos [ Loop (sel_cond, cmd_list) ]
        | Shift (n, lst, p) -> begin
            (* let pos_ptr = Pos.from_sel layout sel in
            let pos_ptr_prev = Pos.shift (-layout_lst.elm_size) pos_ptr in *)
            let pos_ptr = Sel.index_on_itself lst p 0 |> Pos.from_sel layout in
            let pos_ptr_prev = Sel.index_on_itself lst p (-1) |> Pos.from_sel layout in
            match n with
            | 0 -> (pos, [])
            | 1 ->
                let code_move = Pos.gen_bf_move pos pos_ptr in
                let code = code_move @ [ Bf.Code.Add 1 ] in
                (pos_ptr_prev, code)
            | -1 ->
                let code_move = Pos.gen_bf_move pos pos_ptr_prev in
                let code = code_move @ [ Bf.Code.Add (-1) ] in
                (pos_ptr, code)
            | _ -> failwith "not implemented"
          end
        | If (cond, cmd_list_then, cmd_list_else) ->
            let pos_cond = Pos.from_sel layout cond in
            let pos_flag = Pos.shift_last 1 pos_cond in
            let pos_zero = Pos.shift_last 1 pos_flag in
            let pos_then_end, code_then = gen_bf pos_flag cmd_list_then in
            let pos_else_end, code_else = gen_bf pos_flag cmd_list_else in
            let code =
              Pos.gen_bf_move pos pos_flag @
              [
                Bf.Code.Add 1;
                Bf.Code.Shift (-1);
                Bf.Code.Loop (
                  [ Bf.Code.Shift 1; Bf.Code.Add (-1); ] @
                  code_then @
                  Pos.gen_bf_move pos_then_end pos_flag
                );
                Bf.Code.Shift 1;
                Bf.Code.Loop (
                  [ Bf.Code.Add (-1) ] @
                  code_else @
                  Pos.gen_bf_move pos_else_end pos_zero
                );
              ]
            in
            (pos_zero, code)
      ) pos_init code
    in
    (pos, List.flatten bf_cmd_list_list)
  in
  let _, code = gen_bf Pos.init code in
  code
