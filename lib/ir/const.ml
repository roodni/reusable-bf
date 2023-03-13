open Support.Pervasive
open Printf

(* セルの中身が定数になる場合の最適化
   - 条件セルがゼロになるループの除去 (ループ、リセット、一時セル連れ回し)
   - XXX: 関連して実装したい最適化は他にもある
    - 初期化コマンド挿入による生存期間カット
    - once loop(2回目の分岐で必ず抜けるループ)の検出
*)

module ISet = Set.Make(Int)

(** セルが保持しうる数 *)
module Possible = struct
  type t = Just of int | Just2 of int * int | Any

  let add n = function
    | Any -> Any
    | Just x ->
        let x = x + n in
        if 0 <= x && x < 128 then Just x else Any
    | Just2 (x, y) ->
        let x = x + n in
        let y = y + n in
        if 0 <= x && x < 128 && 0 <= y && y < 128
          then Just2 (x, y)
          else Any

  let union v1 v2 =
    let append v s =
      match v with
      | Just n -> ISet.add n s
      | Just2 (n, m) ->
          s |> ISet.add n |> ISet.add m
      | Any -> assert false
    in
    match v1, v2 with
    | Any, _ | _, Any -> Any
    | _ -> begin
        let l =
          ISet.empty |> append v1 |> append v2
          |> ISet.to_seq |> List.of_seq
        in
        match l with
        | [n] -> Just n
        | [n; m] -> Just2 (n, m)
        | _ -> Any
      end

  let zero = Just 0
  let is_zero = function
    | Just 0 -> true
    | _ -> false

  let remove_zero = function
    | Any -> Any
    | Just n -> Just n
    | Just2 (n, m) ->
        if n = 0 then Just m else Just2 (n, m)

  let to_string = function
    | Just n -> string_of_int n
    | Just2 (n, m) -> sprintf "%d|%d" n m
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

  (* 全てがAnyである状態
     findやunionの引数にできる
  *)
  let dummy : t = IdMap.singleton dummy_id NoTracking
  let is_dummy (s: t) = IdMap.mem dummy_id s

  let init Field.{ finite; unlimited }: t =
    let rec init_with_field field state =
      Field.fold
        (fun (id: Id.t) (mtype: Field.mtype) (state: t): t ->
          match mtype with
          | Cell { sticky; _ } ->
              let p =
                (* 配列メンバの非一時セルは解析が面倒なので飛ばす *)
                if sticky then Tracking Possible.zero else NoTracking
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
    if is_dummy state then Possible.Any
    else
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
  let update_f sel f (state: t) : t =
    IdMap.update
      (Sel.last_id sel)
      (function
        | Some NoTracking -> Some NoTracking
        | Some (Tracking p) -> Option.some @@ Tracking (f p)
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
    if is_dummy s1 then s2 else
    if is_dummy s2 then s1 else
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
              !sep (Id.number_only_name id) (Possible.to_string p);
            sep := ", ";
      )
      state;
    Format.fprintf ppf "}";
  ;;
end


type table = {
  (* 直前のコマンドの終了直後の追跡状態 *)
  mutable state_in: State.t;
  (* ループ内の末尾のコマンド終了直後の追跡状態 *)
  mutable state_loop_end: State.t;
}
type code_with_possibles = table Code.t
type analysis_result = code_with_possibles * State.t

let analyze (fmain: Field.main) (code: 'a Code.t): analysis_result =
  let code =
    Code.annot_map
      (fun _ -> {state_in=State.dummy; state_loop_end=State.dummy})
      code
  in
  let rec update_tables (initial_state: State.t) (code: code_with_possibles) : State.t =
    LList.fold_left
      (fun (state: State.t) Code.{ cmd; annot=tbl; _ } : State.t ->
        tbl.state_in <- State.union tbl.state_in state;
        (match cmd with
        | Add (n, sel) -> State.update_f sel (Possible.add n) state
        | Get sel -> State.update sel Possible.Any state
        | Reset sel -> State.update sel Possible.zero state
        | Put _ -> state
        | Shift _ -> state
            (* いまのところ配列メンバの非一時セルを扱わないので無視して良い *)
        | If (cond, thn, els) ->
            let thn_state_out =
              update_tables (State.update_f cond Possible.remove_zero state) thn
            in
            let els_state_out =
              update_tables (State.update cond Possible.zero state) els
            in
            State.union thn_state_out els_state_out
        | IndexIf (_, thn) ->
            let thn_state_out = update_tables state thn in
            State.union state thn_state_out
        | Loop (cond, child) ->
            let rec update_until_fixed_point curr_state =
              (* curr_stateはループに入るときの状態 (条件セルがゼロを含むことがある) *)
              let next_state =
                update_tables
                  (State.update_f cond Possible.remove_zero curr_state)
                  child
                |> State.union curr_state
              in
              if State.equal curr_state next_state
                then next_state
                else update_until_fixed_point next_state
            in
            if Possible.is_zero (State.find cond state) then
              state (* ループに入らない場合 *)
            else
              let state_1 =
                update_tables (State.update_f cond Possible.remove_zero state) child
              in
              let state_01 = State.union state state_1 in
              if Possible.is_zero (State.find cond state_1) then begin
                (* ループを1回で抜ける場合 *)
                tbl.state_loop_end <- State.union tbl.state_loop_end state_1;
                state_01 |> State.update cond Possible.zero
              end else begin
                (* ループが何回か回る場合 *)
                let state_fixed = update_until_fixed_point state_01 in
                tbl.state_loop_end <- State.union tbl.state_loop_end state_fixed;
                state_fixed |> State.update cond Possible.zero
              end
        | IndexLoop (_, child) ->
            let rec update_until_fixed_point curr_state =
              let next_state =
                update_tables curr_state child
                |> State.union curr_state
              in
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
    (fun ppf { state_in; state_loop_end } ->
      fprintf ppf "\t";
      State.output ppf state_in;
      if not (State.is_dummy state_loop_end) then begin
        fprintf ppf "@;<1 2>";
        State.output ppf state_loop_end;
      end
    )
    code;
  fprintf ppf "@,$end\t";
  State.output ppf state_out;
  fprintf ppf "@]";
;;

let insert_reset_before_zero_use (code, _: analysis_result) =
  let rec iter code =
    Code.concat_map
      (fun Code.{ cmd; annot; info } ->
        let { state_in; state_loop_end } = annot in
        match cmd with
        | Add (_, s) | Put s ->
            if Possible.is_zero (State.find s state_in) then
              [`Insert (Code.Reset s, annot); `Keep annot]
            else [`Keep annot]
        | If (cond, _, _) ->
            if Possible.is_zero (State.find cond state_in) then
              [`Insert (Code.Reset cond, annot); `Keep annot]
            else [`Keep annot]
        | Loop (cond, child) ->
            let child =
              if Possible.is_zero (State.find cond state_loop_end) then
                iter child
                @+ llist [ Code.{cmd=Reset cond; annot; info} ]
              else iter child
            in
            let res = [`Insert (Code.Loop (cond, child), annot)] in
            if Possible.is_zero (State.find cond state_in)
              then `Insert (Code.Loop (cond, child), annot) :: res
              else res
        | Shift { index; followers; _ } ->
            LList.fold_left
              (fun code id ->
                let sel = Sel.concat_member_to_index_tail index id 0 in
                if Possible.is_zero (State.find sel state_in) then
                  `Insert (Code.Reset sel, annot) :: code
                else code
              )
              [`Keep annot] followers
        | Reset _ | Get _ | IndexLoop _ | IndexIf _->
            [`Keep annot]
      )
      code
  in
  iter code

let eliminate_never_entered_loop (code, _: analysis_result) =
  Code.concat_map
    (fun Code.{ cmd; annot; _ } ->
      let { state_in; _ } = annot in
      match cmd with
      | Add _ | Put _ | Get _ | IndexLoop _ | If _ | IndexIf _ ->
          [`Keep annot]
      | Reset sel | Loop (sel, _) ->
          if Possible.is_zero (State.find sel state_in)
            then []
            else [`Keep annot]
      | Shift { n; index; followers } ->
          let followers =
            LList.filter
              (fun id ->
                let sel = Sel.concat_member_to_index_tail index id 0 in
                not @@ Possible.is_zero (State.find sel state_in)
              )
              followers
          in
          [`Insert (Shift {n; index; followers}, annot)]
    )
    code