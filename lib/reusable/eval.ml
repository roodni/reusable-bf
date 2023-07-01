open Support.Pervasive
open Support.Info
open Syntax
open Value

let matches pat value =
  let (let*) = Option.bind in
  let rec matches env pat value =
    match pat.v, value with
    | PatVar var, _ -> Some (VE.extend var value env)
    | PatWild, _ -> Some env
    | PatCons (phd, ptl), VaList (vhd :: vtl) ->
        let* env = matches env phd vhd in
        matches env ptl (VaList vtl)
    | PatList pl, VaList vl -> begin
        try
          List.fold_left2
            (fun env p v ->
              matches env p v |> Option.get
            ) env pl vl
          |> Option.some
        with Invalid_argument _ -> None
          (* サイズが合わないかマッチしなければ例外で脱出 *)
      end
    | PatPair (pf, ps), VaPair (vf, vs) ->
        let* env = matches env pf vf in
        matches env ps vs
    | PatInt pi, VaInt vi when pi = vi -> Some env
    | PatBool pb, VaBool vb when pb = vb -> Some env
    | PatUnit, VaUnit -> Some env
    | ( PatCons _ | PatList _ | PatPair _
        | PatInt _ | PatBool _ | PatUnit
      ), _ -> None
  in
  matches VE.empty pat value

let rec eval_let_binding ~recn (envs: envs) ((pat, expr) : let_binding) =
  (* この recn は呼び出し元の eval で増やす *)
  let v = eval ~recn envs expr in
  let matched_env = matches pat v in
  match matched_env with
  | None -> Error.at (merge_info pat.i expr.i) @@ Eval_Match_failed
  | Some env -> env

and eval ~recn (envs: envs) (expr: expr) : value =
  let { i=info; v=expr } = expr in
  let eval_tail envs expr =
    eval ~recn envs expr
  in
  let recn () =
    let n = recn + 1 in
    if n > 50000 then
      Error.at info Memory_Recursion_limit;
    n
  in
  let eval_mid envs expr =
    eval ~recn:(recn ()) envs expr
  in
  match expr with
  | ExVar (uvl, v) -> begin
      let envs =
        List.fold_left
          (fun envs uv -> match UVE.lookup uv envs.module_env with
            | None -> Error.at info @@ Eval_Module_not_defined uv
            | Some m -> m
          ) envs uvl
      in
      match VE.lookup v envs.va_env with
      | Some va -> va
      | None -> Error.at info @@ Eval_Variable_not_defined v
    end
  | ExUnit -> VaUnit
  | ExInt i -> VaInt i
  | ExBool b -> VaBool b
  | ExStr s -> VaString s
  | ExSelMem (parent_ex, offset_ex_opt, var) -> begin
      let offset = match offset_ex_opt with
        | None -> 0
        | Some ex -> eval_mid envs ex |> Va.to_int ex.i
      in
      let parent_sel, idx_id_opt, irid_env =
        eval_mid envs parent_ex |> Va.to_member_selectable parent_ex.i
      in
      match VE.lookup var irid_env with
      | None -> Error.at info @@ Eval_Member_not_defined var
      | Some (id, mtype) -> begin
          let sel =
            Ir.Sel.concat_member_to_tail parent_sel idx_id_opt (Ir.Sel.Member id) offset
          in
          match mtype with
          | MtyCell -> VaCellSel sel
          | MtyArray { mem; _ } -> VaArraySel (sel, mem)
          | MtyIndex -> Error.at info @@ Eval_Member_is_index var
        end
    end
  | ExSelIdx (parent_ex, var) -> begin
      let sel, irid_env = eval_mid envs parent_ex |> Va.to_array parent_ex.i in
      match VE.lookup var irid_env with
      | None -> Error.at info @@ Eval_Member_not_defined var
      | Some (id, mtype) -> begin
          match mtype with
          | MtyIndex -> VaIndexSel ((sel, id), irid_env)
          | MtyCell | MtyArray _ ->
              Error.at info @@ Eval_Member_is_not_index var
        end
    end
  | ExFun (var, ex) -> VaFun (envs, var, ex)
  | ExApp (fn_ex, arg_ex) -> begin
      let fn_va = eval_mid envs fn_ex in
      let arg_va = eval_mid envs arg_ex in
      match fn_va with
      | VaFun (fn_envs, arg_pat, body_ex) -> begin
          let arg_env = matches arg_pat arg_va in
          match arg_env with
          | None -> Error.at info @@ Eval_Match_failed
          | Some arg_env ->
              let envs = Envs.extend_with_value_env arg_env fn_envs in
              eval_tail envs body_ex
        end
      | VaBuiltin fn -> fn (withinfo arg_ex.i arg_va)
      | _ -> Error.at fn_ex.i @@ Eval_Wrong_data_type "function"
    end
  | ExBlock st_list -> VaBlock (envs, st_list)
  | ExBOpInt (ex_left, bop, ex_right) -> begin
      let left = eval_mid envs ex_left |> Va.to_int ex_left.i in
      let right = eval_mid envs ex_right |> Va.to_int ex_right.i in
      match bop with
      | Add -> VaInt (left + right)
      | Sub -> VaInt (left - right)
      | Mul -> VaInt (left * right)
      | Div ->
          if right = 0 then Error.at info Eval_Zero_division
          else VaInt (left / right)
      | Mod ->
          if right = 0 then Error.at info Eval_Zero_division
          else VaInt (left mod right)
      | Lt -> VaBool (left < right)
      | Leq -> VaBool (left <= right)
      | Gt -> VaBool (left > right)
      | Geq -> VaBool (left >= right)
    end
  | ExAnd (ex1, ex2) ->
      let va1 = eval_mid envs ex1 |> Va.to_bool ex1.i in
      if va1 then eval_tail envs ex2
      else VaBool false
  | ExOr (ex1, ex2) ->
      let va2 = eval_mid envs ex1 |> Va.to_bool ex1.i in
      if va2 then VaBool true
      else eval_tail envs ex2
  | ExMinus ex_int ->
      let i = eval_mid envs ex_int |> Va.to_int ex_int.i in
      VaInt (-i)
  | ExEqual (neg, ex_left, ex_right) -> begin
      let left = eval_mid envs ex_left in
      let right = eval_mid envs ex_right in
      match Va.equal left right, neg with
      | Some b, `Eq -> VaBool b
      | Some b, `Neq -> VaBool (not b)
      | None, _ -> Error.at info Eval_Equal_failed
    end
  | ExIf (ex_cond, ex_then, ex_else) ->
      let cond = eval_mid envs ex_cond |> Va.to_bool ex_cond.i in
      eval_tail envs (if cond then ex_then else ex_else)
  | ExLet (binding, expr) ->
      let matched_env = eval_let_binding ~recn:(recn ()) envs binding in
      let envs = Envs.extend_with_value_env matched_env envs in
      eval_tail envs expr
  | ExCons (ex_head, ex_tail) ->
      let head = eval_mid envs ex_head in
      let tail = eval_mid envs ex_tail |> Va.to_list ex_tail.i in
      VaList (head :: tail)
  | ExList el ->
      let vll = List.map (eval_mid envs) el in
      VaList vll
  | ExMatch (matched_ex, clauses) -> begin
      let matched_va = eval_mid envs matched_ex in
      let env_ex_opt =
        List.find_map
          (fun (pat, ex) ->
            matches pat matched_va
            |> Option.map (fun env -> (env, ex)))
          clauses
      in
      match env_ex_opt with
      | Some (env, ex) ->
          let envs = Envs.extend_with_value_env env envs in
          eval_tail envs ex
      | None -> Error.at info @@ Eval_Match_failed
    end
  | ExPair (ex1, ex2) ->
      let v1 = eval_mid envs ex1 in
      let v2 = eval_mid envs ex2 in
      VaPair (v1, v2)