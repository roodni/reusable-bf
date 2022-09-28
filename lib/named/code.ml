(** 中間言語のコード
    解析のため追加情報を付与できる
*)
type 'a t = 'a annotated list

and 'a annotated = { cmd:'a cmd; annot:'a }
and 'a cmd =
  | Add of int * Sel.t
  | Put of Sel.t
  | Get of Sel.t
  | Shift of int * Sel.t * Id.t
  | Loop of Sel.t * 'a t
  | LoopPtr of (Sel.t * Id.t * 'a t)
  | If of Sel.t * 'a t * 'a t

let rec cmd_annot_map f = function
  | Add (n, sel) -> Add (n, sel)
  | Put sel -> Put sel
  | Get sel -> Get sel
  | Shift (n, sel, id) -> Shift (n, sel, id)
    (* ↑コンストラクタで新しい値を構成しないと型が合わない *)
  | Loop (sel, code) ->
      Loop (sel, annot_map f code)
  | LoopPtr (sel, id, code) ->
      LoopPtr (sel, id, annot_map f code)
  | If (sel, thn, els) ->
      If (sel, annot_map f thn, annot_map f els)
and  annot_map f code =
  List.map
    (fun { annot; cmd } ->
      { annot=f annot; cmd=cmd_annot_map f cmd })
    code

let delete_annot code = annot_map (Fun.const ()) code

let from_list cmd_list =
  List.map
    (fun cmd -> { cmd=cmd_annot_map (Fun.const ()) cmd; annot=() })
    cmd_list