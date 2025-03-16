open Info
open Syntax

module VE = Env.Make(Var)
module UVE = Env.Make(UVar)

(* VarとIr.Idの対応、Fieldをコード生成することで得られる *)
type irid_env = (Ir.Id.t * mtype) VE.t
and mtype =
  | MtyCell
  | MtyIndex
  | MtyArray of { length: int option; mem: irid_env }

type value =
  | VaUnit
  | VaInt of int
  | VaBool of bool
  | VaFun of envs * (pat * expr) list
  | VaBuiltin of (trace -> value withinfo -> value)
      (* 関数適用のトレース -> 引数とその位置 -> 返値 *)
  | VaBlock of envs * stmts
  | VaList of value list
  | VaString of string
  | VaTuple of value list
  | VaCellSel of Ir.Sel.t
  | VaArraySel of Ir.Sel.t * irid_env
  | VaIndexSel of Ir.Sel.index * irid_env
and va_env = value VE.t
and module_env = envs UVE.t
and envs =
  { va_env : va_env;
    module_env : module_env;
    trace : trace;
  }

module Va = struct
  type t = value

  let raise_wdt trace info tname =
    Error.at (push_info info trace) @@ Eval_Wrong_data_type tname
  let to_unit tr i = function
    | VaUnit -> ()
    | _ -> raise_wdt tr i "unit"
  let to_int tr i = function
    | VaInt i -> i
    | _ -> raise_wdt tr i "int"
  let to_bool tr i = function
    | VaBool b -> b
    | _ -> raise_wdt tr i "bool"
  let to_block tr i = function
    | VaBlock (env, block) -> (env, block)
    | _ -> raise_wdt tr i "statements"
  let to_list tr i = function
    | VaList l -> l
    | _ -> raise_wdt tr i "list"
  let to_string tr i = function
    | VaString s -> s
    | _ -> raise_wdt tr i "string"
  let to_cell tr i = function
    | VaCellSel sel -> sel
    | _ -> raise_wdt tr i "cell selector"
  let to_index tr i = function
    | VaIndexSel (idx, _) -> idx
    | _ -> raise_wdt tr i "index selector"
  let to_array tr i = function
    | VaArraySel (sel, irid_env) -> (sel, irid_env)
    | _ -> raise_wdt tr i "array selector"
  let to_member_selectable tr i = function
    | VaArraySel (sel, irid_env) -> (sel, None, irid_env)
    | VaIndexSel ((sel, id), irid_env) -> (sel, Some id, irid_env)
    | _ -> raise_wdt tr i "array or index selector"

  let equal x y =
    let rec loop = function
      | [] -> Some true
      | (x, y) :: rest -> begin
          match x, y with
          | VaUnit, VaUnit -> Some true
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
          | VaTuple xl, VaTuple yl ->
              if List.(length xl <> length yl) then None
              else
                (* タプルは頑張って大量に手書きしないとstack overflowしないので対策は不要 *)
                let rest = List.rev_append (List.combine xl yl) rest in
                loop rest
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
      trace = empty_trace;
    }
  let initial =
    let add_builtin name fn env =
      VE.extend (Var.of_string name) (VaBuiltin fn) env
    in
    { empty with
      va_env = empty.va_env
        |> add_builtin "string_length" (fun trace s ->
            let l = 
              Va.to_string trace s.i s.v
              |> String.length
            in
            VaInt l
          )
        |> add_builtin "string_get" (fun trace s ->
            let s = Va.to_string trace s.i s.v in
            VaBuiltin (fun trace i ->
              let i = Va.to_int trace i.i i.v in
              let c =
                try int_of_char s.[i] with
                | Invalid_argument _ ->
                    Error.at trace (Eval_Exception "index out of bounds")
              in
              VaInt c
            )
          )
        |> add_builtin "failwith" (fun trace msg ->
            let msg = Va.to_string trace msg.i msg.v in
            Error.at trace (Eval_Exception msg)
          )
      ;
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
      (fun env (var, (id, mtype)) ->
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
      trace = dest.trace;
    }
end