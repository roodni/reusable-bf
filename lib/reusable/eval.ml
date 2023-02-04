open Support.Pervasive
open Support.Info
open Syntax

module VE = Env.Make(Var)
module UVE = Env.Make(UVar)

(* VarとIr.Idの対応、Fieldをコード生成することで得られる *)
type irid_env = (Ir.Id.t * mtype) withinfo VE.t
and mtype =
  | MtyCell
  | MtyIndex
  | MtyArray of { length: int option; mem: irid_env }

type value =
  | VaInt of int
  | VaBool of bool
  | VaFun of envs * pat * expr
  | VaBlock of envs * stmts
  | VaList of value list
  | VaPair of value * value
  | VaCellSel of Ir.Sel.t
  | VaArraySel of Ir.Sel.t * irid_env
  | VaIndexSel of Ir.Sel.index * irid_env
and va_env = value VE.t
and module_env = envs UVE.t
and envs =
  { va_env : va_env;
    module_env : module_env;
  }

let empty_envs =
  { va_env = VE.empty;
    module_env = UVE.empty;
  }
let update_envs_with_va_env va_env envs =
  { envs with va_env = VE.merge va_env envs.va_env }
let add_module_binding_to_envs uv mod_envs envs =
  { envs with
    module_env = UVE.extend uv mod_envs envs.module_env
  }

(** [import_envs src dest] *)
let import_envs src dest =
  { va_env = VE.merge src.va_env dest.va_env;
    module_env = UVE.merge src.module_env dest.module_env;
  }

module Value = struct
  let to_int info = function
    | VaInt i -> i
    | _ -> Error.at info @@ Eval_Wrong_data_type "int"
  let to_bool info = function
    | VaBool b -> b
    | _ -> Error.at info @@ Eval_Wrong_data_type "bool"
  let to_block info = function
    | VaBlock (env, block) -> (env, block)
    | _ -> Error.at info @@ Eval_Wrong_data_type "statements"
  let to_list info = function
    | VaList l -> l
    | _ -> Error.at info @@ Eval_Wrong_data_type "list"
  let to_pair info = function
    | VaPair (v1, v2) -> (v1, v2)
    | _ -> Error.at info @@ Eval_Wrong_data_type "pair"
  let to_cell info = function
    | VaCellSel sel -> sel
    | _ -> Error.at info @@ Eval_Wrong_data_type "cell selector"
  let to_index info = function
    | VaIndexSel (idx, _) -> idx
    | _ -> Error.at info @@ Eval_Wrong_data_type "index selector"
  let to_array info = function
    | VaArraySel (sel, irid_env) -> (sel, irid_env)
    | _ -> Error.at info @@ Eval_Wrong_data_type "array selector"
  let to_member_selectable info = function
    | VaArraySel (sel, irid_env) -> (sel, None, irid_env)
    | VaIndexSel ((sel, id), irid_env) -> (sel, Some id, irid_env)
    | _ -> Error.at info @@ Eval_Wrong_data_type "array or index selector"

  let equal x y =
    let rec loop = function
      | [] -> Some true
      | (x, y) :: rest -> begin
          match x, y with
          | VaInt x, VaInt y -> if x = y then loop rest else Some false
          | VaBool x, VaBool y -> if x = y then loop rest else Some false
          | VaCellSel x, VaCellSel y
          | VaArraySel (x, _), VaArraySel (y, _) -> if x = y then loop rest else Some false
          | VaIndexSel (x, _), VaIndexSel (y, _) -> if x = y then loop rest else Some false
          | VaList xl, VaList yl ->
              if List.(length xl <> length yl) then Some false
              else
                let rest =
                  List.fold_left2 (fun r x y -> (x, y) :: r) rest xl yl
                in
                loop rest
          | VaPair (x1, x2), VaPair (y1, y2) -> loop ((x1, y1) :: (x2, y2) :: rest)
          | _ -> None
        end
    in
    loop [(x, y)]

  let extend_env_with_irid_env
      (diving: Ir.Sel.index option) (irid_env: irid_env) (env: va_env) =
    Seq.fold_left
      (fun env (var, { v=(id, mtype); i=_ }) ->
        match mtype with
        | MtyCell ->
            let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
            VE.extend var (VaCellSel sel) env
        | MtyArray { mem; _ } ->
            let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
            VE.extend var (VaArraySel (sel, mem)) env
        | MtyIndex -> env
            (* 既存のフィールド確保の枠組みでは
               直下にindexのあるフィールドは確保されない *)
      )
      env
      (VE.to_seq irid_env)
end


let matches pat value =
  let (let*) = Option.bind in
  let rec matches env pat value =
    match pat.v, value with
    | PatVar var, _ -> Some (VE.extend var value env)
    | PatWild, _ -> Some env
    | PatCons (phd, ptl), VaList (vhd :: vtl) ->
        let* env = matches env phd vhd in
        matches env ptl (VaList vtl)
    | PatNil, VaList [] -> Some env
    | PatPair (pf, ps), VaPair (vf, vs) ->
        let* env = matches env pf vf in
        matches env ps vs
    | PatInt pi, VaInt vi when pi = vi -> Some env
    | PatBool pb, VaBool vb when pb = vb -> Some env
    | _ -> None
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
  let open Value in
  match expr with
  | ExVar (uvl, v) -> begin
      let envs =
        LList.fold_left
          (fun envs uv -> match UVE.lookup uv envs.module_env with
            | None -> Error.at info @@ Eval_Module_not_defined uv
            | Some m -> m
          ) envs uvl
      in
      match VE.lookup v envs.va_env with
      | Some va -> va
      | None -> Error.at info @@ Eval_Variable_not_defined v
    end
  | ExInt i -> VaInt i
  | ExBool b -> VaBool b
  | ExStr s ->
      VaList
        ( String.to_seq s
          |> Seq.map (fun c -> VaInt (int_of_char c))
          |> List.of_seq )
  | ExSelMem (parent_ex, offset_ex_opt, var) -> begin
      let offset = match offset_ex_opt with
        | None -> 0
        | Some ex -> eval_mid envs ex |> to_int ex.i
      in
      let parent_sel, idx_id_opt, irid_env =
        eval_mid envs parent_ex |> to_member_selectable parent_ex.i
      in
      match VE.lookup var irid_env with
      | None -> Error.at info @@ Eval_Member_not_defined var
      | Some { v=(id, mtype); _ } -> begin
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
      let sel, irid_env = eval_mid envs parent_ex |> to_array parent_ex.i in
      match VE.lookup var irid_env with
      | None -> Error.at info @@ Eval_Member_not_defined var
      | Some { v=(id, mtype); _ } -> begin
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
              let envs = update_envs_with_va_env arg_env fn_envs in
              eval_tail envs body_ex
        end
      (* | VaBuiltin Fst -> to_pair ex_arg.i va_arg |> fst *)
      | _ -> Error.at fn_ex.i @@ Eval_Wrong_data_type "function"
    end
  | ExBlock st_list -> VaBlock (envs, st_list)
  | ExBOpInt (ex_left, bop, ex_right) -> begin
      let left = eval_mid envs ex_left |> to_int ex_left.i in
      let right = eval_mid envs ex_right |> to_int ex_right.i in
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
      let va1 = eval_mid envs ex1 |> to_bool ex1.i in
      if va1 then eval_tail envs ex2
      else VaBool false
  | ExOr (ex1, ex2) ->
      let va2 = eval_mid envs ex1 |> to_bool ex1.i in
      if va2 then VaBool true
      else eval_tail envs ex2
  | ExMinus ex_int ->
      let i = eval_mid envs ex_int |> to_int ex_int.i in
      VaInt (-i)
  | ExEqual (neg, ex_left, ex_right) -> begin
      let left = eval_mid envs ex_left in
      let right = eval_mid envs ex_right in
      match equal left right, neg with
      | Some b, `Eq -> VaBool b
      | Some b, `Neq -> VaBool (not b)
      | None, _ -> Error.at info Eval_Equal_failed
    end
  | ExIf (ex_cond, ex_then, ex_else) ->
      let cond = eval_mid envs ex_cond |> to_bool ex_cond.i in
      eval_tail envs (if cond then ex_then else ex_else)
  | ExLet (binding, expr) ->
      let matched_env = eval_let_binding ~recn:(recn ()) envs binding in
      let envs = update_envs_with_va_env matched_env envs in
      eval_tail envs expr
  | ExCons (ex_head, ex_tail) ->
      let head = eval_mid envs ex_head in
      let tail = eval_mid envs ex_tail |> to_list ex_tail.i in
      VaList (head :: tail)
  | ExList el ->
      let vll = LList.map (eval_mid envs) el in
      VaList (LList.to_list_danger vll)
  | ExMatch (matched_ex, clauses) -> begin
      let matched_va = eval_mid envs matched_ex in
      let env_ex_opt =
        LList.find_map
          (fun (pat, ex) ->
            matches pat matched_va
            |> Option.map (fun env -> (env, ex)))
          clauses
      in
      match env_ex_opt with
      | Some (env, ex) ->
          let envs = update_envs_with_va_env env envs in
          eval_tail envs ex
      | None -> Error.at info @@ Eval_Match_failed
    end
  | ExPair (ex1, ex2) ->
      let v1 = eval_mid envs ex1 in
      let v2 = eval_mid envs ex2 in
      VaPair (v1, v2)