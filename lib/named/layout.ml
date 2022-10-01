open Support.Pervasive

(** 変数名と確保されたテープ位置の対応 *)
type t = (Id.t * loc) list
and loc =
  | Cell of { offset: int; is_index: bool; }
  | Ifable of { offset_of_cond: int; cond_to_else: int; else_to_endif: int; }
  | Array of {
      offset_of_body: int; (* メンバを格納するエリアの左端 (さらに左側にはインデックスだけを含むヘッダがある) *)
      size_of_members: int;
      members: t;
      length: int option;
    }

let lookup (layout: t) id = List.assoc id layout

(** セル割り付け *)
let from_field (code: 'a Code.t) (field: Field.main): t =
  (* 左から詰める *)
  let mcounter = MovementCounter.from_code code in
  let rec allocate (field: Field.t) (ofs_available: int): t * int =
    (* 割り付け先送りコストを格納するテーブル *)
    let cost_table =
      Field.fold
        (fun id _ l ->
          let initial_cost =
            MovementCounter.get mcounter MovementCounter.leftside id
              - MovementCounter.get mcounter MovementCounter.rightside id
              - MovementCounter.get mcounter Field.uarray_id id
          in
          (id, initial_cost) :: l
        )
        field []
      |> List.to_seq |> Hashtbl.of_seq
    in
    (* テーブルから優先順に取り出して配置する *)
    let rec pickup_loop ~ifable_space_allocated layout ofs_available =
      (* mtype > 先送りコスト > 定義順 で最優先のものを取り出す *)
      let precedence id cost =
        let mtype_precedence =
          match Field.lookup field id with
          | Field.Array _ -> 90
          | Field.Cell { ifable=true; _ } when ifable_space_allocated -> 60
          | Field.Cell _ -> 30
          | Field.Index -> 20
        in
        ( mtype_precedence,
          cost,
          - Id.to_definition_order id
        )
      in
      let top =
        Hashtbl.fold
          (fun next_id next_cost curr ->
            let next_precedence = precedence next_id next_cost in
            let next = Some (next_id, next_precedence) in
            match curr with
            | None -> next
            | Some (_, curr_precedence) ->
                if next_precedence > curr_precedence then next else curr
          )
          cost_table None
      in
      (* セルを配置する *)
      match top with
      | None ->
          (* 全てのセルを配置した *)
          (layout, ofs_available)
      | Some (top_id, _) ->
          (* コストのテーブルを更新する *)
          Hashtbl.remove cost_table top_id;
          Hashtbl.to_seq_keys cost_table
            |> List.of_seq
            |> List.iter
              (fun id ->
                Hashtbl.add_assign_int cost_table id
                  (MovementCounter.get mcounter top_id id)
              );
          (* 左に詰める *)
          let top_mtype = Field.lookup field top_id in
          let top_loc, ofs_available =
            match top_mtype with
            | Field.Cell { ifable=false; _ } ->
                (Cell { offset=ofs_available; is_index=false }, ofs_available + 1)
            | Field.Cell { ifable=true; _ } ->
                if ifable_space_allocated then
                  (Ifable { offset_of_cond=ofs_available; cond_to_else=(-1); else_to_endif=(-1) }, ofs_available + 1)
                else
                  (Ifable { offset_of_cond=ofs_available; cond_to_else=1; else_to_endif=1 }, ofs_available + 3)
            | Index ->
                (Cell { offset=ofs_available; is_index=true }, ofs_available + 1)
            | Array { members; length; } ->
                (* メンバを配置する *)
                let layout_members, size_of_members = allocate members 0 in
                (* メンバのインデックスで最も左にあるものの位置 *)
                let ofs_first_index =
                  layout_members
                  |> List.filter_map
                    (fun (_, loc) ->
                      match loc with
                      | Cell { is_index=true; offset } -> Some offset
                      | _ -> None )
                  |> List.fold_left min size_of_members
                in
                let offset_of_body = ofs_available + size_of_members - ofs_first_index in
                ( Array {
                    members = layout_members;
                    length = (if top_id = Field.uarray_id then None else Some length);
                    size_of_members; offset_of_body;
                  },
                  offset_of_body + size_of_members * length
                )
          in
          let ifable_space_allocated =
            not ifable_space_allocated &&
            match top_mtype with Field.Cell { ifable=true; _ } -> true | _ -> false
          in
          pickup_loop ~ifable_space_allocated ((top_id, top_loc) :: layout) ofs_available
    in
    pickup_loop ~ifable_space_allocated:false [] ofs_available
  in
  (* 有限の構造と無限配列に分けて配置する *)
  let Field.{ finite; unlimited } = field in
  let layout_finite, ofs_uarray_start = allocate finite 0 in
  let uarray =
    (Field.uarray_id, Field.Array { members=unlimited; length=1 })
    |> Seq.return |> Hashtbl.of_seq
  in
  let layout_uarray, _ = allocate uarray ofs_uarray_start in
  layout_finite @ layout_uarray

let show ppf layout =
  let loc_to_offset = function
    | Cell { offset; _ } -> offset
    | Ifable { offset_of_cond; _ } -> offset_of_cond
    | Array { offset_of_body; _ } -> offset_of_body
  in
  let open Format in
  let rec show layout =
    let ordered_members =
      layout |>
        List.filter
          (fun (_, loc) ->
            match loc with
            | Array { size_of_members=0; _ } -> false
            | _ -> true )
          |>
        List.map (fun (id, loc) -> (loc_to_offset loc, (id, loc))) |>
        List.sort
          (fun (ofs1, _) (ofs2, _) -> Int.compare ofs1 ofs2)
    in
    fprintf ppf "{@;<1 2>@[<hv>";
    List.iteri
      (fun i (ofs, (id, loc)) ->
        if i > 0 then pp_print_space ppf ();
        fprintf ppf "%d -> @[<hv>%s: " ofs (Id.numbered_name id);
        ( match loc with
          | Cell { is_index=false; _ } -> fprintf ppf "cell";
          | Cell { is_index=true; _ } -> fprintf ppf "index";
          | Ifable _ -> fprintf ppf "cell?";
          | Array { size_of_members; members; length; _ } ->
              let length_s = match length with
                | Some l -> string_of_int l
                | None -> "_"
              in
              fprintf ppf "array(%s) <%d>" length_s size_of_members;
              show members
        );
        fprintf ppf ";@]";
      )
      ordered_members;
    fprintf ppf "@]@ }";
  in
  show layout;