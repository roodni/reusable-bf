open Support.Pervasive
open Printf

(* 抽象解釈による最適化
   - XXX: 関連して実装したい最適化は他にもある
    - 2回目の分岐で必ず抜けるループの検出
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


module IdMap = Map.Make(Id)
module State = struct

  type elt = Tracking of Possible.t * Sel.t | NoTracking
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
          | Cell { sticky; index; _ } ->
              let p =
                if sticky then
                  let sel = Sel.concat_member_to_index_opt_tail index id 0 in
                  Tracking (Possible.zero, sel)
                else NoTracking
                  (* 配列メンバの非一時セルは解析が面倒なので飛ばす *)
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
      | Tracking (p, _) -> p
      | NoTracking -> Possible.Any

  let fold (f: 'a -> Possible.t -> Sel.t -> 'a) init (state: t) =
    IdMap.fold
      (fun _ elt accu ->
        match elt with
        | Tracking (possible, sel) -> f accu possible sel
        | NoTracking -> accu
      )
      state init

  (* セレクタの指すセルの保持しうる数を変更した追跡状態を返す
     ただし非追跡のセレクタの場合は無視する (そのうち全てを追跡対象にしたい)
  *)
  let update sel possible (state: t) : t =
    IdMap.update
      (Sel.last_id sel)
      (function
        | Some NoTracking -> Some NoTracking
        | Some (Tracking (_, sel)) -> Some (Tracking (possible, sel))
        | None -> assert false
      )
      state
  let update_f sel f (state: t) : t =
    IdMap.update
      (Sel.last_id sel)
      (function
        | Some NoTracking -> Some NoTracking
        | Some (Tracking (p, sel)) -> Option.some @@ Tracking (f p, sel)
        | None -> assert false
      )
      state

  let equal (s1: t) (s2: t) =
    IdMap.equal
      (fun e1 e2 ->
        match e1, e2 with
        | Tracking (p1, _), Tracking (p2, _) -> Possible.equal p1 p2
        | NoTracking, NoTracking -> true
        | _ -> false)
      s1 s2

  let union (s1: t) (s2: t) : t =
    if is_dummy s1 then s2 else
    if is_dummy s2 then s1 else
    IdMap.union
      (fun _ e1 e2 ->
        match e1, e2 with
        | Tracking (p1, sel), Tracking (p2, _) ->
            Some (Tracking (Possible.union p1 p2, sel))
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
        | Tracking (Possible.Any, _) -> ()
        | Tracking (p, _) ->
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
  (* 子コードの末尾のコマンド終了直後の追跡状態 *)
  mutable state_block_end: State.t;
  (* ?のelse用 *)
  mutable state_else_end: State.t;
}
type code_with_possibles = table Code.t
type analysis_result = code_with_possibles * State.t

let analyze (fmain: Field.main) (code: 'a Code.t): analysis_result =
  let code =
    Code.annot_map
      (fun _ -> {
        state_in=State.dummy;
        state_block_end=State.dummy;
        state_else_end=State.dummy;
      })
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
        | Put _ | Use _ -> state
        | Shift _ -> state
            (* いまのところ配列メンバの非一時セルを扱わないので無視して良い *)
        | If (cond, thn, els) ->
            let thn_state_out =
              update_tables (State.update_f cond Possible.remove_zero state) thn
            in
            tbl.state_block_end <- State.union tbl.state_block_end thn_state_out;
            let els_state_out =
              update_tables (State.update cond Possible.zero state) els
            in
            tbl.state_else_end <- State.union tbl.state_else_end els_state_out;
            State.union thn_state_out els_state_out
        | IndexIf (_, thn) ->
            let thn_state_out = update_tables state thn in
            tbl.state_block_end <- State.union tbl.state_block_end thn_state_out;
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
                tbl.state_block_end <- State.union tbl.state_block_end state_1;
                state_01 |> State.update cond Possible.zero
              end else begin
                (* ループが何回か回る場合 *)
                let state_fixed = update_until_fixed_point state_01 in
                tbl.state_block_end <- State.union tbl.state_block_end state_fixed;
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
            let state_fixed = update_until_fixed_point state in
            tbl.state_block_end <- State.union tbl.state_block_end state_fixed;
            state_fixed
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
    (fun ppf { state_in; state_block_end; _ } ->
      fprintf ppf "\t";
      State.output ppf state_in;
      if not (State.is_dummy state_block_end) then begin
        fprintf ppf "@;<1 2>";
        State.output ppf state_block_end;
      end
    )
    code;
  fprintf ppf "@,$end\t";
  State.output ppf state_out;
  fprintf ppf "@]";
;;

let insert_reset_before_zero_use code get_const get_liveness =
  let zero_cells live_in state_in =
    (* ゼロであり、かつ生存しているセルを列挙する *)
    State.fold
      (fun zero_cells possible sel ->
        if Possible.is_zero possible
            && Liveness.CellSet.mem (Sel.last_id sel) live_in
          then sel :: zero_cells else zero_cells
      )
      [] state_in
    |> llist
  in
  let rec iter code =
    Code.concat_map
      (fun Code.{ cmd; annot; info } ->
        let reset_cells cells =
          LList.map
            (fun sel -> Code.{cmd=Reset sel; annot; info})
            cells
        in
        let reset_cells_insertion cells before =
          LList.fold_left
            (fun accu sel ->
              `Insert (Code.Reset sel, annot) :: accu)
            before cells
        in
        let { state_in; state_block_end; state_else_end } = get_const annot in
        let Liveness.{ live_in; live_out } = get_liveness annot in
        match cmd with
        | Add (_, s) | Put s | Use s ->
            if Possible.is_zero (State.find s state_in) then
              [`Insert (Code.Reset s, annot); `Keep annot]
            else [`Keep annot]
        | If (cond, code_then, code_else) ->
            let zero_cells_then = zero_cells live_out state_block_end in
            let zero_cells_else = zero_cells live_out state_else_end in
            let cmd =
              Code.If
                ( cond,
                  iter code_then @+ reset_cells zero_cells_then,
                  iter code_else @+ reset_cells zero_cells_else )
            in
            let res = `Insert (cmd, annot) in
            if Possible.is_zero (State.find cond state_in) then
              [`Insert (Code.Reset cond, annot); res]
            else [res]
        | IndexIf (cond, code_then) ->
            let zero_cells_then = zero_cells live_out state_block_end in
            let cmd =
              Code.IndexIf
                ( cond, iter code_then @+ reset_cells zero_cells_then)
            in
            [`Insert (cmd, annot)]
        | Loop (cond, child) ->
            (* ループ終わりのゼロ初期化挿入 *)
            let zero_cells_loop_end = zero_cells live_in state_block_end in
            let child =
              iter child @+ reset_cells zero_cells_loop_end
            in
            (* ループ前のゼロ初期化挿入 *)
            let zero_cells_init = zero_cells live_in state_in in
            reset_cells_insertion zero_cells_init
              [`Insert (Code.Loop (cond, child), annot)]
        | IndexLoop (cond, child) ->
            let zero_cells_loop_end = zero_cells live_in state_block_end in
            let child =
              iter child @+ reset_cells zero_cells_loop_end
            in
            let zero_cells_init = zero_cells live_in state_in in
            reset_cells_insertion zero_cells_init
              [`Insert (Code.IndexLoop (cond, child), annot)]
        | Shift { index; followers; _ } ->
            LList.fold_left
              (fun code id ->
                let sel = Sel.concat_member_to_index_tail index id 0 in
                if Possible.is_zero (State.find sel state_in) then
                  `Insert (Code.Reset sel, annot) :: code
                else code
              )
              [`Keep annot] followers
        | Reset _ | Get _ ->
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
      | Add _ | Put _ | Get _ | IndexLoop _ | If _ | IndexIf _ | Use _ ->
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