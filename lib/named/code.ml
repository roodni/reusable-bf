(** 中間言語のコード
    解析のため追加情報を付与できる
*)
type 'a t = 'a annotated list
and 'a annotated = { cmd:'a cmd; annot:'a }
and 'a cmd =
  | Add of int * Sel.t
  | Put of Sel.t
  | Get of Sel.t
  | Shift of { n:int; index:(Sel.t * Id.t); followers:Id.t list }
  | Loop of Sel.t * 'a t
  | LoopIndex of (Sel.t * Id.t * 'a t)
  | If of Sel.t * 'a t * 'a t
  | Reset of Sel.t

let rec cmd_annot_map f = function
  | Add (n, sel) -> Add (n, sel)
  | Put sel -> Put sel
  | Get sel -> Get sel
  | Reset sel -> Reset sel
  | Shift params -> Shift params
    (* ↑コンストラクタで新しい値を構成しないと型が合わない *)
  | Loop (sel, code) ->
      Loop (sel, annot_map f code)
  | LoopIndex (sel, id, code) ->
      LoopIndex (sel, id, annot_map f code)
  | If (sel, thn, els) ->
      If (sel, annot_map f thn, annot_map f els)
and annot_map f code =
  List.map
    (fun { annot; cmd } ->
      { annot=f annot; cmd=cmd_annot_map f cmd })
    code

let delete_annot code = annot_map (Fun.const ()) code

let from_list cmd_list =
  List.map
    (fun cmd -> { cmd=cmd_annot_map (Fun.const ()) cmd; annot=() })
    cmd_list

let shift_followers n (arr_sel, idx_id) followers =
  followers
  |> List.map
    (fun follower_id ->
      let src_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) follower_id 0 in
      let dest_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) follower_id n in
      [ Loop
          ( src_sel,
            [ Add (-1, src_sel);
              Add (1, dest_sel);
            ] |> from_list
          )
      ]
    )
  |> List.flatten |> from_list

let desugar_LoopIndex (arr_sel, idx_id, loop) =
  let cond_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) idx_id (-1) in
  from_list [ Loop (cond_sel, loop) ]