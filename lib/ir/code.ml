open Support.Pervasive

(** 中間言語のコード
    解析のため追加情報を付与できる
*)
type 'a t = 'a annotated llist
and 'a annotated = { cmd:'a cmd; annot:'a }
and 'a cmd =
  | Add of int * Sel.t
  | Put of Sel.t
  | Get of Sel.t
  | Shift of { n:int; index:Sel.index; followers:Id.t llist }
  | Loop of Sel.t * 'a t
  | If of Sel.t * 'a t * 'a t
  | IndexLoop of (Sel.index * 'a t)
  | IndexIf of (Sel.index * 'a t)
  | Reset of Sel.t

(** アノテーション付きコマンド1つ単位で単純な書き換えを行う
    構造を維持して再帰的に書き換えることも可能
*)
let rec filter_map
    (f: 'a annotated -> [`Update of 'b annotated | `Keep of 'b | `Delete])
    (code: 'a t) : 'b t =
  LList.filter_map
    (fun annotated ->
      match f annotated with
      | `Update annotated -> Some annotated
      | `Keep annot -> Some { cmd=cmd_filter_map f annotated.cmd; annot }
      | `Delete -> None
    )
    code
and cmd_filter_map f = function
  | Add (n, sel) -> Add (n, sel)
  | Put sel -> Put sel
  | Get sel -> Get sel
  | Reset sel -> Reset sel
  | Shift params -> Shift params
    (* ↑コンストラクタで新しい値を構成しないと型が合わない *)
  | Loop (sel, code) ->
      Loop (sel, filter_map f code)
  | IndexLoop (index, code) ->
      IndexLoop (index, filter_map f code)
  | If (sel, thn, els) ->
      If (sel, filter_map f thn, filter_map f els)
  | IndexIf (index, code) ->
      IndexIf (index, filter_map f code)

(** アノテーションだけを書き換える *)
let annot_map f code =
  filter_map (fun { annot; _ } -> `Keep (f annot)) code
let cmd_annot_map f cmd =
  cmd_filter_map (fun { annot; _ } -> `Keep (f annot)) cmd

let delete_annot code = annot_map (Fun.const ()) code


let from_cmd_llist cmd_llist : unit t =
  LList.map
    (fun cmd -> { cmd=cmd_annot_map (Fun.const ()) cmd; annot=() })
    cmd_llist
let from_cmds cmd_list = from_cmd_llist (llist cmd_list)

(** アノテーション込みでコードを出力する *)
let output ppf output_annot code =
  let open Format in
  let rec print_code code =
    let print_block code =
      fprintf ppf "[ @[<v>";
      print_code code;
      fprintf ppf " ]@]";
    in
    LList.iteri
      (fun i { cmd; annot } ->
        if i > 0 then fprintf ppf "@,";
        (match cmd with
        | Add (n, sel) ->
            let cmdc = if n >= 0 then '+' else '-' in
            fprintf ppf "%c %s %d" cmdc (Sel.to_string sel) (abs n);
            output_annot ppf annot;
        | Put sel ->
            fprintf ppf ". %s" (Sel.to_string sel);
            output_annot ppf annot;
        | Get sel ->
            fprintf ppf ", %s" (Sel.to_string sel);
            output_annot ppf annot;
        | Reset sel ->
            fprintf ppf "$reset %s" (Sel.to_string sel);
            output_annot ppf annot;
        | Shift { n; index=(arr_sel, idx_id); _ } ->
            let cmdc = if n >= 0 then '>' else '<' in
            fprintf ppf "%c %s@%s %d"
              cmdc (Sel.to_string arr_sel) (Id.simple_name idx_id) (abs n);
            output_annot ppf annot;
        | Loop (sel, code) ->
            fprintf ppf "! %s" (Sel.to_string sel);
            output_annot ppf annot;
            fprintf ppf "@;<0 2>";
            print_block code;
        | IndexLoop (index, code) ->
            fprintf ppf "<!> %s" (Sel.index_to_string index);
            output_annot ppf annot;
            fprintf ppf "@;<0 2>";
            print_block code;
        | If (sel, thn_code, els_code) ->
            fprintf ppf "? %s" (Sel.to_string sel);
            output_annot ppf annot;
            fprintf ppf "@;<0 2>";
            print_block thn_code;
            fprintf ppf "@;<0 2>";
            print_block els_code;
        | IndexIf (index, code) ->
            fprintf ppf "<?> %s" (Sel.index_to_string index);
            output_annot ppf annot;
            fprintf ppf "@;<0 2>";
            print_block code;
        )
      )
      code
  in
  fprintf ppf "@[<v>";
  print_code code;
  fprintf ppf "@]";
;;


let shift_followers n (arr_sel, idx_id) (followers: Id.t llist) =
  followers
  |> LList.map
    (fun follower_id ->
      let src_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) follower_id 0 in
      let dest_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) follower_id n in
      llist [
        Loop
          ( src_sel,
            from_cmds
              [ Add (-1, src_sel); Add (1, dest_sel); ]
          )
      ]
    )
  |> LList.concat |> from_cmd_llist


let extend_IndexLoop ((arr_sel, idx_id), loop) =
  let cond_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) idx_id (-1) in
  from_cmds [ Loop (cond_sel, loop) ]

let extend_IndexIf (index, code) =
  let _, idx_id = index in
  let idx_sel = Sel.concat_member_to_index_tail index idx_id 0 in
  let prev_idx_sel = Sel.concat_member_to_index_tail index idx_id (-1) in
  from_cmds [
    Loop (prev_idx_sel, from_cmds [
      Add (-1, prev_idx_sel);
      Add (1, idx_sel);
    ]);
    Loop (idx_sel,
      from_cmds [
        Add (-1, idx_sel);
        Add (1, prev_idx_sel);
      ]
      @+ delete_annot code
    )
  ]


(** イディオム[-]を専用コマンドに変換する *)
let convert_idioms code =
  filter_map
    (fun { cmd; _ } -> match cmd with
      | Loop (sel1, child) -> begin
          match LList.to_list_danger child with
          | [ {cmd=Add (-1, sel2); _} ] when sel1 = sel2 ->
              `Update { cmd=Reset sel1; annot=() }
          | _ -> `Keep ()
        end
      | Add _ | Put _ | Get _ | Shift _ | Reset _
      | IndexLoop _ | If _ | IndexIf _ ->
          `Keep ()
    )
    code