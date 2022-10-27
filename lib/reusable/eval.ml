open Support.Pervasive
open Support.Info
open Syntax

module VE = Env.Make(Var)
module UVE = Env.Make(UVar)

type value =
  | VaInt of int
  | VaBool of bool
  | VaFun of envs * pat * expr
  | VaBlock of envs * stmts
  | VaList of value list
  | VaPair of value * value
  | VaCellSel of Ir.Sel.t
  | VaArraySel of Ir.Sel.t * IrIdEnv.t
  | VaIndexSel of Ir.Sel.index * IrIdEnv.t
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

let export_envs { va_env; module_env; } =
  { va_env = VE.export va_env;
    module_env = UVE.export module_env;
  }
let import_envs src dest =
  { va_env = VE.import src.va_env dest.va_env;
    module_env = UVE.import src.module_env dest.module_env;
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
      (diving: Ir.Sel.index option) (irid_env: IrIdEnv.t) (env: va_env) =
    LList.fold_left
      (fun env (var, { v=(id, mtype); i=_ }) ->
        match mtype with
        | IrIdEnv.Cell ->
            let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
            VE.extend var (VaCellSel sel) env
        | Array { mem; _ } ->
            let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
            VE.extend var (VaArraySel (sel, mem)) env
        | Index -> env
            (* 既存のフィールド確保の枠組みでは
               直下にindexのあるフィールドは確保されない *)
      )
      env
      (IrIdEnv.to_llist irid_env)
end


let rec matches ~export va_env pat value =
  let (let*) = Option.bind in
  match pat.v, value with
  | PatVar var, _ -> Some (VE.extend ~export var value va_env)
  | PatWild, _ -> Some va_env
  | PatCons (phd, ptl), VaList (vhd :: vtl) ->
      let* va_env = matches ~export va_env phd vhd in
      matches ~export va_env ptl (VaList vtl)
  | PatNil, VaList [] -> Some va_env
  | PatPair (pf, ps), VaPair (vf, vs) ->
      let* va_env = matches ~export va_env pf vf in
      matches ~export va_env ps vs
  | PatInt pi, VaInt vi when pi = vi -> Some va_env
  | PatBool pb, VaBool vb when pb = vb -> Some va_env
  | _ -> None

let rec eval_let_binding ~export ~recn (envs: envs) ((pat, expr) : let_binding) =
  (* この recn は呼び出し元の eval で増やす *)
  let v = eval ~recn envs expr in
  let va_env_opt = matches ~export envs.va_env pat v in
  match va_env_opt with
  | None -> Error.at (merge_info pat.i expr.i) @@ Eval_Match_failed
  | Some va_env -> { envs with va_env }

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
  | ExVar v -> begin
      match VE.lookup v envs.va_env with
      | Some va -> va
      | None -> Error.at info @@ Eval_Variable_not_defined v
    end
  | ExModuleVar (uv, v) -> begin
      match UVE.lookup uv envs.module_env with
      | None -> Error.at info @@ Eval_Module_not_defined uv
      | Some m -> begin
          match VE.lookup v m.va_env with
          | Some v -> v
          | None -> Error.at info @@ Eval_Variable_not_defined v
        end
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
      match IrIdEnv.lookup var irid_env with
      | None -> Error.at info @@ Eval_Member_not_defined var
      | Some { v=(id, mtype); _ } -> begin
          let sel =
            Ir.Sel.concat_member_to_tail parent_sel idx_id_opt (Ir.Sel.Member id) offset
          in
          match mtype with
          | IrIdEnv.Cell -> VaCellSel sel
          | Array { mem; _ } -> VaArraySel (sel, mem)
          | Index -> Error.at info @@ Eval_Member_is_index var
        end
    end
  | ExSelIdx (parent_ex, var) -> begin
      let sel, irid_env = eval_mid envs parent_ex |> to_array parent_ex.i in
      match IrIdEnv.lookup var irid_env with
      | None -> Error.at info @@ Eval_Member_not_defined var
      | Some { v=(id, mtype); _ } -> begin
          match mtype with
          | Index -> VaIndexSel ((sel, id), irid_env)
          | Cell | Array _ ->
              Error.at info @@ Eval_Member_is_not_index var
        end
    end
  | ExFun (var, ex) -> VaFun (envs, var, ex)
  | ExApp (ex_fn, ex_arg) -> begin
      let va_fn = eval_mid envs ex_fn in
      let va_arg = eval_mid envs ex_arg in
      match va_fn with
      | VaFun (envs_fun, pat_arg, ex_body) -> begin
          let env_opt = matches ~export:false envs_fun.va_env pat_arg va_arg in
          match env_opt with
          | None -> Error.at info @@ Eval_Match_failed
          | Some va_env -> eval_tail { envs_fun with va_env } ex_body
        end
      (* | VaBuiltin Fst -> to_pair ex_arg.i va_arg |> fst *)
      | _ -> Error.at ex_fn.i @@ Eval_Wrong_data_type "function"
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
      let envs_let =
        eval_let_binding ~export:false ~recn:(recn ()) envs binding
      in
      eval_tail envs_let expr
  | ExCons (ex_head, ex_tail) ->
      let head = eval_mid envs ex_head in
      let tail = eval_mid envs ex_tail |> to_list ex_tail.i in
      VaList (head :: tail)
  | ExList el ->
      let vll = LList.map (eval_mid envs) el in
      VaList (LList.to_list_danger vll)
  | ExMatch (ex_matched, pat_ex_list) -> begin
      let va_matched = eval_mid envs ex_matched in
      let env_ex_opt =
        LList.find_map
          (fun (pat, ex) ->
            matches ~export:false envs.va_env pat va_matched
            |> Option.map (fun va_env -> (va_env, ex)))
          pat_ex_list
      in
      match env_ex_opt with
      | Some (va_env, ex) -> eval_tail { envs with va_env } ex
      | None -> Error.at info @@ Eval_Match_failed
    end
  | ExPair (ex1, ex2) ->
      let v1 = eval_mid envs ex1 in
      let v2 = eval_mid envs ex2 in
      VaPair (v1, v2)