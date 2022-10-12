open Printf
open Support.Error
open Syntax

module VE = Env.Make(Var)
module UVE = Env.Make(UVar)

type value =
  | VaInt of int
  | VaBool of bool
  | VaFun of envs * pat * expr
  | VaBlock of envs * stmt list
  | VaList of value list
  | VaPair of value * value
  | VaCellSel of Ir.Sel.t
  | VaArraySel of Ir.Sel.t * IrIdEnv.t
  | VaIndexSel of Ir.Sel.index * IrIdEnv.t
and va_env = value VE.t
and module_env = envs UVE.t
and envs = {
    va_env : va_env;
    module_env : module_env;
  }

let empty_envs = {
  va_env = VE.empty;
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
    | _ -> error_at info "An int value expected"
  let to_bool info = function
    | VaBool b -> b
    | _ -> error_at info "A bool value expected"
  let to_block info = function
    | VaBlock (env, block) -> (env, block)
    | _ -> error_at info "A block statement value expected"
  let to_list info = function
    | VaList l -> l
    | _ -> error_at info "A list value expected"
  let to_pair info = function
    | VaPair (v1, v2) -> (v1, v2)
    | _ -> error_at info "A pair value expected"
  let to_cell info = function
    | VaCellSel sel -> sel
    | _ -> error_at info "A cell selector value expected"
  let to_index info = function
    | VaIndexSel (idx, _) -> idx
    | _ -> error_at info "An index selector value expected"
  let to_array info = function
    | VaArraySel (sel, irid_env) -> (sel, irid_env)
    | _ -> error_at info "An array selector value expected"
  let to_member_selectable info = function
    | VaArraySel (sel, irid_env) -> (sel, None, irid_env)
    | VaIndexSel ((sel, id), irid_env) -> (sel, Some id, irid_env)
    | _ -> error_at info "An array or index selector value expected"
  (* TODO: whileのための急工事、気持ち悪いのですぐ直す *)
  let to_cell_or_index info = function
    | VaCellSel sel -> `Cell sel
    | VaIndexSel (idx, _) -> `Index idx
    | _ -> error_at info "A cell or index selector value expected"

  let equal x y =
    let rec equal x y =
      match x, y with
      | VaInt x, VaInt y -> x = y
      | VaBool x, VaBool y -> x = y
      | VaCellSel x, VaCellSel y
      | VaArraySel (x, _), VaArraySel (y, _) -> x = y
      | VaIndexSel (x, _), VaIndexSel (y, _) -> x = y
      | VaList x, VaList y -> begin
          try List.for_all2 equal x y with
          | Invalid_argument _ -> false
        end
      | VaPair (x1, x2), VaPair (y1, y2) -> equal x1 y1 && equal x2 y2
      | _ -> raise Exit
    in
    try Some (equal x y) with
    | Exit -> None

  let extend_env_with_irid_env
      (diving: Ir.Sel.index option) (irid_env: IrIdEnv.t) (env: va_env) =
    List.fold_left
      (fun env (var, { v=(id, mtype); i }) ->
        let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
        match mtype with
        | IrIdEnv.Cell ->
            VE.extend var (VaCellSel sel) env
        | Index ->
            error_at i "Index must be declared as a member of an array"
        | Array { mem; _ } ->
            VE.extend var (VaArraySel (sel, mem)) env
      )
      env
      (IrIdEnv.to_list irid_env)
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

let rec eval_let_binding ~export (envs: envs) ((pat, expr) : let_binding) =
  let v = eval envs expr in
  let va_env_opt = matches ~export envs.va_env pat v in
  match va_env_opt with
  | None -> error_at (merge_info pat.i expr.i) "Match failed"
  | Some va_env -> { envs with va_env }

and eval (envs: envs) (expr: expr) : value =
  let open Value in
  let { i = info; v = expr } = expr in
  match expr with
  | ExVar v -> begin
      match VE.lookup v envs.va_env with
      | Some va -> va
      | None -> error_at info @@ sprintf "Unbound value '%s'" (Var.to_string v)
    end
  | ExModuleVar (uv, v) -> begin
      match UVE.lookup uv envs.module_env with
      | None -> error_at info @@ sprintf "Unbound module '%s'" (UVar.to_string uv)
      | Some m -> begin
          match VE.lookup v m.va_env with
          | None -> error_at info @@ sprintf "Unbound value '%s'" (Var.to_string v)
          | Some v -> v
        end
    end
  | ExInt i -> VaInt i
  | ExBool b -> VaBool b
  | ExStr s -> VaList (String.to_seq s |> Seq.map (fun c -> VaInt (int_of_char c)) |> List.of_seq)
  | ExSelMem (parent_ex, offset_ex_opt, var) -> begin
      let offset = match offset_ex_opt with
        | None -> 0
        | Some ex -> eval envs ex |> to_int ex.i
      in
      let parent_sel, idx_id_opt, irid_env =
        eval envs parent_ex |> to_member_selectable parent_ex.i
      in
      match IrIdEnv.lookup var irid_env with
      | None -> error_at info @@ sprintf "Unbound member '%s'" (Var.to_string var)
      | Some { v=(id, mtype); _ } -> begin
          let sel =
            Ir.Sel.concat_member_to_tail parent_sel idx_id_opt (Ir.Sel.Member id) offset
          in
          match mtype with
          | IrIdEnv.Cell -> VaCellSel sel
          | Array { mem; _ } -> VaArraySel (sel, mem)
          | Index ->
              error_at info @@
                sprintf "The member '%s' is an index (Use '@' instead of ':')"
                  (Var.to_string var)
        end
    end
  | ExSelIdx (parent_ex, var) -> begin
      let sel, irid_env = eval envs parent_ex |> to_array parent_ex.i in
      match IrIdEnv.lookup var irid_env with
      | None -> error_at info @@ sprintf "Unbound member '%s'" (Var.to_string var)
      | Some { v=(id, mtype); _ } -> begin
          match mtype with
          | Index -> VaIndexSel ((sel, id), irid_env)
          | Cell | Array _ ->
              error_at info @@
                sprintf "The member '%s' is not an index (Use ':' instead of '@')"
                  (Var.to_string var)
        end
    end
  | ExFun (var, ex) -> VaFun (envs, var, ex)
  | ExApp (ex_fn, ex_arg) -> begin
      let va_fn = eval envs ex_fn in
      let va_arg = eval envs ex_arg in
      match va_fn with
      | VaFun (envs_fun, pat_arg, ex_body) -> begin
          let env_opt = matches ~export:false envs_fun.va_env pat_arg va_arg in
          match env_opt with
          | None -> error_at info "match failed"
          | Some va_env -> eval { envs_fun with va_env } ex_body
        end
      (* | VaBuiltin Fst -> to_pair ex_arg.i va_arg |> fst *)
      | _ -> error_at ex_fn.i "function expected"
    end
  | ExBlock st_list -> VaBlock (envs, st_list)
  | ExBOpInt (ex_left, bop, ex_right) -> begin
      let left = eval envs ex_left |> to_int ex_left.i in
      let right = eval envs ex_right |> to_int ex_right.i in
      match bop with
      | Add -> VaInt (left + right)
      | Sub -> VaInt (left - right)
      | Mul -> VaInt (left * right)
      | Div -> VaInt (left / right)
      | Mod -> VaInt (left mod right)
      | Lt -> VaBool (left < right)
      | Leq -> VaBool (left <= right)
    end
  | ExMinus ex_int ->
      let i = eval envs ex_int |> to_int ex_int.i in
      VaInt (-i)
  | ExEqual (ex_left, ex_right) -> begin
      let left = eval envs ex_left in
      let right = eval envs ex_right in
      match equal left right with
      | Some b -> VaBool b
      | None -> error_at info "Cannot test equality"
    end
  | ExIf (ex_cond, ex_then, ex_else) ->
      let cond = eval envs ex_cond |> to_bool ex_cond.i in
      eval envs (if cond then ex_then else ex_else)
  | ExLet (binding, expr) ->
      let envs_let = eval_let_binding ~export:false envs binding in
      eval envs_let expr
  | ExNil -> VaList []
  | ExCons (ex_head, ex_tail) ->
      let head = eval envs ex_head in
      let tail = eval envs ex_tail |> to_list ex_tail.i in
      VaList (head :: tail)
  | ExList el ->
      let vl = List.map (eval envs) el in
      VaList vl
  | ExMatch (ex_matched, pat_ex_list) -> begin
      let va_matched = eval envs ex_matched in
      let env_ex_opt =
        List.find_map
          (fun (pat, ex) ->
            matches ~export:false envs.va_env pat va_matched
            |> Option.map (fun va_env -> (va_env, ex)))
          pat_ex_list
      in
      match env_ex_opt with
      | Some (va_env, ex) -> eval { envs with va_env } ex
      | None -> error_at info "Match failed"
    end
  | ExPair (ex1, ex2) ->
      let v1 = eval envs ex1 in
      let v2 = eval envs ex2 in
      VaPair (v1, v2)

let load_program filename =
  let file_in = open_in filename in
  let lexbuf = Lexer.create filename file_in in
  let program =
    try Parser.program Lexer.main lexbuf with
    | Lexer.Error info -> error_at info "Syntax error"
    | Parser.Error -> begin
        let info = !Lexer.curr_info in
        error_at info "Unexpected token"
      end
  in
  let () = close_in file_in in
  program

let rec eval_toplevels curr_dirname import_history envs toplevels =
  let import_module info filename =
    let filename =
      if FilePath.is_relative filename then
        FilePath.concat curr_dirname filename
        |> FilePath.reduce ~no_symlink:true
      else filename
    in
    let next_dirname = FilePath.dirname filename in
    if List.mem filename import_history then
      error_at info "Recursive import"
    else
      let toplevels, _ = load_program filename in
      eval_toplevels next_dirname (filename :: import_history) empty_envs toplevels
  in
  List.fold_left
    (fun envs toplevel ->
      match toplevel.v with
      | TopLet binding -> eval_let_binding ~export:true envs binding
      | TopImport filename ->
          let envs_imported = import_module toplevel.i filename in
          import_envs envs_imported envs
      | TopImportAs (filename, uv) ->
          let envs_imported = import_module toplevel.i filename |> export_envs in
          { envs with module_env = UVE.extend uv envs_imported envs.module_env }
    )
    envs toplevels