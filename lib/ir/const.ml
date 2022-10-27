open Support.Pervasive

(* セルの中身が定数になる場合の最適化
   - 条件セルがゼロになるループの除去 (ループ、リセット、一時セル連れ回し)
   - XXX: 関連して実装したい最適化は他にもある
    - 初期化コマンド挿入による生存期間カット
    - once loop(2回目の分岐で必ず抜けるループ)の検出
*)

(** セルが保持しうる数 *)
module Possible = struct
  type t = Just of int | Any

  let add n = function
    | Any -> Any
    | Just m ->
        let x = n + m in
        if 0 <= x && x < 128 then Just x else Any

  let union v1 v2 =
    match v1, v2 with
    | Just n1, Just n2 when n1 = n2 -> Just n1
    | _ -> Any

  let to_string = function
    | Just n -> string_of_int n
    | Any -> "any"

  (** 「セルが保持しうる値」が同じであることを判定する
      セルの中身が等しいことは保証しない *)
  let equal (v1: t) (v2: t) = v1 = v2
end


module State = struct
  module IdMap = Map.Make(Id)

  type elt = Tracking of Possible.t | NoTracking
  type t = elt IdMap.t

  let dummy_id = Id.gen_special "DUMMY"
  let dummy : t = IdMap.singleton dummy_id NoTracking
  let is_dummy (s: t) = IdMap.mem dummy_id s

  let init Field.{ finite; unlimited }: t =
    let rec init_with_field field state =
      Field.fold
        (fun (id: Id.t) (mtype: Field.mtype) (state: t): t ->
          match mtype with
          | Cell { mergeable; _ } ->
              let p =
                (* 配列メンバの非一時セルは解析が面倒なので飛ばす *)
                if mergeable then Tracking (Possible.Just 0) else NoTracking
              in
              IdMap.add id p state
          | Index -> state
          | Array { members; _ } -> state |> init_with_field members
        )
        field
        state
    in
    IdMap.empty
    |> init_with_field finite
    |> init_with_field unlimited

  let find sel (state: t) =
    match IdMap.find (Sel.last_id sel) state with
    | Tracking p -> p
    | NoTracking -> Possible.Any

  (* セレクタの指すセルの保持しうる数を変更した追跡状態を返す
     ただし非追跡のセレクタの場合は無視する (そのうち全てを追跡対象にしたい)
  *)
  let update sel possible (state: t) : t =
    IdMap.update
      (Sel.last_id sel)
      (function
        | Some NoTracking -> Some NoTracking
        | Some (Tracking _) -> Some (Tracking possible)
        | None -> assert false
      )
      state

  let equal (s1: t) (s2: t) =
    IdMap.equal
      (fun e1 e2 ->
        match e1, e2 with
        | Tracking p1, Tracking p2 -> Possible.equal p1 p2
        | NoTracking, NoTracking -> true
        | _ -> false)
      s1 s2

  let union (s1: t) (s2: t) : t =
    IdMap.union
      (fun _ e1 e2 ->
        match e1, e2 with
        | Tracking p1, Tracking p2 -> Some (Tracking (Possible.union p1 p2))
        | NoTracking, NoTracking -> Some NoTracking
        | _ -> assert false
      )
      s1 s2

  let output ppf (state: t) =
    let sep = ref "" in
    Format.fprintf ppf "{";
    IdMap.iter
      (fun id elt ->
        match elt with
        | NoTracking -> ()
        | Tracking Possible.Any -> ()
        | Tracking p ->
            Format.fprintf ppf "%s%s->%s"
              !sep (Id.numbered_name id) (Possible.to_string p);
            sep := ", ";
      )
      state;
    Format.fprintf ppf "}";
  ;;
end


type table = {
  (* 直前のコマンドの終了直後のセルのとりうる値の追跡状態
     ループの場合、入れ子になったコマンド終了直後の状態はここでは扱わない
     (once loop検出のため対応する予定がある)
  *)
  mutable state_in: State.t;
}
type code_with_possibles = table Code.t
type analysis_result = code_with_possibles * State.t

let analyze (fmain: Field.main) (code: 'a Code.t): analysis_result =
  let code = Code.annot_map (fun _ -> { state_in=State.dummy }) code in
  let rec update_tables (initial_state: State.t) (code: code_with_possibles) : State.t =
    LList.fold_left
      (fun (state: State.t) Code.{ cmd; annot=tbl } : State.t ->
        tbl.state_in <-
          if State.is_dummy tbl.state_in
            then state
            else State.union tbl.state_in state;
        (match cmd with
        | Add (n, sel) ->
            State.update sel
              (State.find sel state |> Possible.add n)
              state
        | Get sel -> State.update sel (Possible.Any) state
        | Reset sel -> State.update sel (Possible.Just 0) state
        | Put _ -> state
        | Shift _ -> state
            (* いまのところ配列メンバの非一時セルを扱わないので無視して良い *)
        | If (cond, thn, els) ->
            let thn_state_out = update_tables state thn in
            let els_state_out =
              update_tables (State.update cond (Possible.Just 0) state) els
            in
            State.union thn_state_out els_state_out
        | Loop (cond, child) ->
            let rec update_until_fixed_point curr_state =
              let cond_p = State.find cond curr_state in
              if Possible.equal cond_p (Possible.Just 0) then curr_state
              else
                let next_state = update_tables curr_state child in
                if State.equal curr_state next_state
                  then next_state |> State.update cond (Possible.Just 0)
                  else update_until_fixed_point next_state
            in
            update_until_fixed_point state
        | ILoop (_, child) ->
            let rec update_until_fixed_point curr_state =
              let next_state = update_tables curr_state child in
              if State.equal curr_state next_state
                then next_state
                else update_until_fixed_point next_state
            in
            update_until_fixed_point state
        )
      )
      initial_state
      code
  in
  let state_out = update_tables (State.init fmain) code in
  (code, state_out)

let output_analysis_result ppf (code, state_out: analysis_result) =
  let open Format in
  fprintf ppf "@[<v>";
  Code.output ppf
    (fun ppf { state_in } ->
      fprintf ppf "\t";
      State.output ppf state_in;
    )
    code;
  fprintf ppf "@,$end\t";
  State.output ppf state_out;
  fprintf ppf "@]";
;;

let eliminate_never_entered_loop (code, _: analysis_result) =
  Code.filter_map
    (fun Code.{ cmd; annot } ->
      let { state_in } = annot in
      match cmd with
      | Add _ | Put _ | Get _ | ILoop _ | If _ ->
          `Keep annot
      | Reset sel | Loop (sel, _) ->
          if Possible.equal (State.find sel state_in) (Possible.Just 0)
            then `Delete
            else `Keep annot
      | Shift { n; index; followers } ->
          let followers =
            LList.filter
              (fun id ->
                let sel = Sel.concat_member_to_index_tail index id 0 in
                not @@ Possible.equal (State.find sel state_in) (Possible.Just 0)
              )
              followers
          in
          `Update { cmd=Shift { n; index; followers }; annot }
    )
    code