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

module Va = struct
  type t = value
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
end

module Envs = struct
  type t = envs

  let empty =
    { va_env = VE.empty;
      module_env = UVE.empty;
    }
  let extend_with_value_env va_env envs =
    { envs with va_env = VE.merge va_env envs.va_env }
  let add_module_binding uv mod_envs envs =
    { envs with
      module_env = UVE.extend uv mod_envs envs.module_env
    }

  let extend_value_env_with_irid_env
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

  (** [import src dest] *)
  let import src dest =
    { va_env = VE.merge src.va_env dest.va_env;
      module_env = UVE.merge src.module_env dest.module_env;
    }
end