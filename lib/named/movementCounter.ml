open Printf
open Support.Pervasive

(** フィールドの並びを最適化するため、
    フィールドのメンバのペアごとに移動回数をカウントする
*)
type t = (Id.t * Id.t, int) Hashtbl.t

(* 初期位置や配列の添字付きメンバなど、明確に左か右にあるメンバを表すid *)
let leftside = Id.gen_named "[left]"
let rightside = Id.gen_named "[right]"

let add (tbl: t) id1 id2 = Hashtbl.add_assign_int tbl (id1, id2) 1
let dec (tbl: t) id1 id2 = Hashtbl.add_assign_int tbl (id1, id2) (-1)

let get (tbl: t) id1 id2 =
  Hashtbl.find_default tbl (id1, id2) 0 +
  Hashtbl.find_default tbl (id2, id1) 0

let dump (tbl: t) =
  let tbl = Hashtbl.copy tbl in
  Hashtbl.to_seq_keys tbl
  |> List.of_seq |> List.sort compare
  |> List.iter
    (fun (origin, dest) ->
      if Hashtbl.mem tbl (origin, dest) then begin
        printf "(%s, %s) -> %d\n"
          (Id.to_string origin)
          (Id.to_string dest)
          (get tbl origin dest);
        Hashtbl.remove tbl (origin, dest);
        Hashtbl.remove tbl (dest, origin);
      end
    )

let rec add_id_to_sel_until_index (tbl: t) id sel =
  match sel with
  | Sel.Member m ->
      if m <> id then add tbl id m;
      sel
  | Array { name; index_opt=None; member; _ } ->
      add tbl id name;
      add_id_to_sel_until_index tbl id member
  | Array { name; index_opt=Some idx; _ } ->
      add tbl id name;
      add tbl id idx;
      sel

let rec add_sel_updown (tbl: t) (dir: [`Up | `Down]) sel =
  match sel with
  | Sel.Member _ -> ()
  | Array { index_opt=None; _ } -> assert false
  | Array { index_opt=Some idx; offset; member; _ } ->
      let offset = offset - (match dir with `Up -> 1 | `Down -> 0) in
      let idx_dest, mem_origin =
        if offset > 0 then (rightside, leftside) else
        if offset < 0 then (leftside, rightside)
        else (idx, idx)
      in
      let next_idx_or_mem_sel =
        if idx_dest <> idx then add tbl idx idx_dest;
        add_id_to_sel_until_index tbl mem_origin member;
      in
      add_sel_updown tbl dir next_idx_or_mem_sel

let rec add_sel_to_sel (tbl: t) (origin: Sel.t) (dest: Sel.t) =
  match origin, dest with
  | Array { name=o_name; index_opt=o_idx_opt; offset=o_offset; member=o_member },
    Array { name=d_name; index_opt=d_idx_opt; offset=d_offset; member=d_member }
    when o_name = d_name -> begin
    (* 両者のセレクタの表層が同じ配列である *)
      match o_idx_opt, d_idx_opt with
      | _ when o_idx_opt = d_idx_opt ->
        (* インデックスが同じ、または両方ともインデックスでない *)
          if o_offset = d_offset then
            (* インデックスとオフセットが同じならその配列を無視できる *)
            add_sel_to_sel tbl o_member d_member
          else begin
            (* オフセットが違うなら影響を考慮する *)
            let origin_to, dest_from =
              if o_offset < d_offset then (rightside, leftside)
                else (leftside, rightside)
            in
            let o_next = add_id_to_sel_until_index tbl origin_to o_member in
            add_sel_updown tbl `Up o_next;
            let d_next = add_id_to_sel_until_index tbl dest_from d_member in
            add_sel_updown tbl `Down d_next;
          end
      | None, None -> assert false
      | None, Some d_idx ->
        (* 下りの表層だけがインデックスである *)
          let origin_to = if o_offset = 0 then d_idx else leftside in
          let o_next = add_id_to_sel_until_index tbl origin_to o_member in
          add_sel_updown tbl `Up o_next;
          add_sel_updown tbl `Down dest;
      | Some _, None ->
        (* 上りの表層だけがインデックスである *)
          add_sel_updown tbl `Up origin;
          let d_next = add_id_to_sel_until_index tbl leftside d_member in
          add_sel_updown tbl `Down d_next;
      | Some o_idx, Some d_idx ->
        (* 両者が異なるインデックスである *)
          add_sel_updown tbl `Up origin;
          add tbl o_idx d_idx;
          add_sel_updown tbl `Down dest;
    end
  | origin, dest ->
    (* 両者のセレクタが同じ配列を指していない *)
      let o_head = Sel.head_id origin in
      let d_head = Sel.head_id dest in
      let o_next = add_id_to_sel_until_index tbl d_head origin in
      add_sel_updown tbl `Up o_next;
      let d_next = add_id_to_sel_until_index tbl o_head dest in
      add_sel_updown tbl `Down d_next;
      (* 表層同士の移動が2重カウントされるのを打ち消す *)
      if o_head <> d_head then dec tbl o_head d_head

let from_code (code: 'a Code.t): t =
  let code = Code.delete_annot code in
  let tbl = Hashtbl.create 200 in
  let rec scan_code initial_sel code =
    List.fold_left
      (fun curr_sel Code.{ cmd; _ } ->
        match cmd with
        | Code.Add (0, _) -> curr_sel
        | Add (_, sel) | Put sel | Get sel | Reset sel ->
            add_sel_to_sel tbl curr_sel sel;
            sel
        | Loop (cond_sel, code) ->
            add_sel_to_sel tbl curr_sel cond_sel;
            let wend_sel = scan_code cond_sel code in
            add_sel_to_sel tbl wend_sel cond_sel;
            cond_sel
        | LoopIndex params ->
            scan_code curr_sel (Code.desugar_LoopIndex params)
        | Shift { n; index; followers } -> begin
            let idx_id = snd index in
            let index_sel = Sel.concat_member_to_index_tail index idx_id 0 in
            let prev_index_sel = Sel.concat_member_to_index_tail index idx_id (-1) in
            let curr_sel =
              scan_code curr_sel (Code.shift_followers n index followers)
            in
            match n with
            | 0 -> curr_sel
            | 1 ->
                add_sel_to_sel tbl curr_sel index_sel;
                prev_index_sel
            | -1 ->
                add_sel_to_sel tbl curr_sel prev_index_sel;
                index_sel
            | _ -> assert false
          end
        | If (cond_sel, code_then, code_else) ->
            add_sel_to_sel tbl curr_sel cond_sel;
            let sel = scan_code cond_sel code_then in
            add_sel_to_sel tbl sel cond_sel;
            let sel = scan_code cond_sel code_else in
            add_sel_to_sel tbl sel cond_sel;
            cond_sel
      )
      initial_sel
      code
  in
  ignore @@ scan_code (Sel.Member leftside) code;
  tbl

(* let from_code _code = Hashtbl.create 30 *)