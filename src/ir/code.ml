open Support.Pervasive
open Support.Info

(** 中間言語のコード
    解析のため追加情報を付与できる
*)
type 'a t = 'a annotated list
and 'a annotated =
  { cmd: 'a cmd;
    annot: 'a;
    trace: trace;
  }
and 'a cmd =
  | Add of int * Sel.t
  | Put of Sel.t
  | Get of Sel.t
  | Shift of { n:int; index:Sel.index; followers:Id.t list }
  | Loop of Sel.t * 'a t
  | If of Sel.t * 'a t * 'a t
  | IndexLoop of (Sel.index * 'a t)
  | IndexIf of (Sel.index * 'a t)
  | Reset of Sel.t
  | Use of Sel.t
      (* 生存情報解析でuse判定になるだけのダミー命令
         allocセルを使用後にゼロクリアするために使う *)

(** アノテーション付きコマンドをそれぞれ0個以上のコマンドで置き換える
    構造を維持して再帰的に書き換えることも可能
*)
let rec concat_map
    (f: 'a annotated -> [`Insert of 'b cmd * 'b | `Keep of 'b] list)
    (code: 'a t) : 'b t =
  List.concat_map
    (fun annotated ->
      (* match f annotated with
      | `Keep annot -> Some { cmd=cmd_filter_map f annotated.cmd; annot; info=annotated.info } *)
      List.map
        (function
          | `Insert (cmd, annot) -> { cmd; annot; trace=annotated.trace }
          | `Keep annot ->
              { cmd=cmd_concat_map f annotated.cmd; annot; trace=annotated.trace } )
        (f annotated)
    )
    code
and cmd_concat_map f = function
  | Add (n, sel) -> Add (n, sel)
  | Put sel -> Put sel
  | Get sel -> Get sel
  | Reset sel -> Reset sel
  | Shift params -> Shift params
    (* ↑コンストラクタで新しい値を構成しないと型が合わない *)
  | Loop (sel, code) ->
      Loop (sel, concat_map f code)
  | IndexLoop (index, code) ->
      IndexLoop (index, concat_map f code)
  | If (sel, thn, els) ->
      If (sel, concat_map f thn, concat_map f els)
  | IndexIf (index, code) ->
      IndexIf (index, concat_map f code)
  | Use (sel) -> Use (sel)

(** アノテーションだけを書き換える *)
let annot_map f code =
  concat_map (fun { annot; _ } -> [`Keep (f annot)]) code
let cmd_annot_map f cmd =
  cmd_concat_map (fun { annot; _ } -> [`Keep (f annot)]) cmd

let delete_annot code = annot_map (Fun.const ()) code


let from_cmds trace cmd_list : unit t =
  List.map
    (fun cmd -> {
      cmd=cmd_annot_map (Fun.const ()) cmd;
      annot=();
      trace;
    })
    cmd_list

(** アノテーション込みでコードを出力する *)
let output ppf output_annot code =
  let open Format in
  let rec print_code code =
    let print_block code =
      fprintf ppf "[ @[<v>";
      print_code code;
      fprintf ppf " ]@]";
    in
    List.iteri
      (fun i { cmd; annot; _ } ->
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
        | Use sel ->
            fprintf ppf "$use %s" (Sel.to_string sel);
            output_annot ppf annot;
        )
      )
      code
  in
  fprintf ppf "@[<v>";
  print_code code;
  fprintf ppf "@]";
;;


let shift_followers trace n (arr_sel, idx_id) (followers: Id.t list) =
  followers
  |> List.map
    (fun follower_id ->
      let src_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) follower_id 0 in
      let dest_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) follower_id n in
      [ Loop
          ( src_sel,
            from_cmds trace
              [ Add (-1, src_sel); Add (1, dest_sel); ]
          )
      ]
    )
  |> List.concat |> from_cmds trace


let extend_IndexLoop trace ((arr_sel, idx_id), loop) =
  let cond_sel = Sel.concat_member_to_index_tail (arr_sel, idx_id) idx_id (-1) in
  from_cmds trace [ Loop (cond_sel, loop) ]

let extend_IndexIf trace (index, code) =
  let _, idx_id = index in
  let idx_sel = Sel.concat_member_to_index_tail index idx_id 0 in
  let prev_idx_sel = Sel.concat_member_to_index_tail index idx_id (-1) in
  from_cmds trace [
    Loop (prev_idx_sel,
      from_cmds trace [
        Add (-1, prev_idx_sel);
        Add (1, idx_sel);
      ]
    );
    Loop (idx_sel,
      from_cmds trace [
        Add (-1, idx_sel);
        Add (1, prev_idx_sel);
      ]
      @ delete_annot code
    )
  ]


(** イディオム[-]を専用コマンドに変換する *)
let convert_idioms code =
  concat_map
    (fun { cmd; _ } ->
      match cmd with
      | Loop (sel1, child) -> begin
          match child with
          | [ {cmd=Add (-1, sel2); _} ] when sel1 = sel2 ->
              [`Insert (Reset sel1, ())]
          | _ -> [`Keep ()]
        end
      | Add _ | Put _ | Get _ | Shift _ | Reset _
      | IndexLoop _ | If _ | IndexIf _
      | Use _ ->
          [`Keep ()]
    )
    code