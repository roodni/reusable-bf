open Batteries
open Printf

module Var = struct
  type t = string
end


module Field = struct
  type t = (Var.t * kind) list
  and kind =
    | Cell
    | Ptr
    | Lst of lst
  and lst = {
    length: int option;
    mem: t;
  }
end

module BOpInt = struct
  type t = Add | Sub | Mul | Div | Mod | Lt | Leq
end

type stmt =
  | StAdd of int * expr * expr option  (* sign, sel, int *)
  | StPut of expr
  | StGet of expr
  | StWhile of expr * stmt list
  | StIf of expr * stmt list * stmt list option
  | StShift of int * expr * expr option  (* sign, ptr, int *)
  | StVar of Field.t * stmt list
  | StLet of Var.t * expr * stmt list
  | StExpand of expr
  | StDive of expr * stmt list
and expr =
  | ExVar of Var.t
  | ExInt of int
  | ExBool of bool
  | ExSelMem of expr * expr option * Var.t
  | ExSelPtr of expr * Var.t
  | ExFun of Var.t * expr
  | ExApp of expr * expr
  | ExBlock of stmt list
  | ExBOpInt of expr * BOpInt.t * expr
  | ExMinus of expr
  | ExEqual of expr * expr
  | ExIf of expr * expr * expr
  | ExLet of Var.t * expr * expr

module Stmt = struct
  type t = stmt
end

module Expr = struct
  type t = expr
end


(** Named.Selをラップする *)
module Sel = struct
  type t =
    | Base of Named.Var.t
    | LstMem of t * int * Named.Var.t
    | LstPtr of t * Named.Var.t
  type nsel_or_nptr =
    | NSel of Named.Sel.t
    | NPtr of Named.Sel.t * Named.Var.t

  let base_or_mem (parent: t option) (v: Named.Var.t) =
    match parent with
    | None -> Base v
    | Some sel -> LstMem (sel, 0, v)

  let rec to_dfn_key = function
    | Base nv -> [ nv ]
    | LstMem (sel, _, nv) -> (to_dfn_key sel) @ [ nv ]
    | LstPtr (sel, _) -> to_dfn_key sel

  let rec to_nsel_or_nptr sel =
    let rec to_nsel_lst index child sel =
      match sel with
      | Base nv -> Named.Sel.Lst (nv, index, child)
      | LstMem (LstPtr (sel, p), i, nv) ->
          let child = Named.Sel.Lst (nv, index, child) in
          to_nsel_lst_ptr p i child sel
      | LstMem (sel, i, nv) ->
          let child = Named.Sel.Lst (nv, index, child) in
          to_nsel_lst i child sel
      | LstPtr _ -> assert false
    and to_nsel_lst_ptr ptr index child sel =
      match sel with
      | Base nv -> Named.Sel.LstPtr (nv, ptr, index, child)
      | LstMem (LstPtr (sel, p), i, nv) ->
          let child = Named.Sel.LstPtr (nv, ptr, index, child) in
          to_nsel_lst_ptr p i child sel
      | LstMem (sel, i, nv) ->
          let child = Named.Sel.LstPtr (nv, ptr, index, child) in
          to_nsel_lst i child sel
      | LstPtr _ -> assert false
    in
    match sel with
    | Base nv -> NSel (Named.Sel.V nv)
    | LstMem (LstPtr (sel, ptr), index, nv) -> NSel (to_nsel_lst_ptr ptr index (Named.Sel.V nv) sel)
    | LstMem (sel, index, nv) -> NSel (to_nsel_lst index (Named.Sel.V nv) sel)
    | LstPtr (sel, ptr) -> begin
        let nsel = to_nsel_or_nptr sel in
        match nsel with
        | NSel nsel -> NPtr (nsel, ptr)
        | NPtr _ -> assert false
      end
  let to_nsel sel =
    match to_nsel_or_nptr sel with
    | NSel nsel -> nsel
    | NPtr _ -> failwith "selector is selecting a pointer"
  let to_nptr sel =
    match to_nsel_or_nptr sel with
    | NSel _ -> failwith "selector is not selecting a pointer"
    | NPtr (nsel, ptr) -> (nsel, ptr)

  let rec has_ptr ptr sel =
    match sel with
    | Base _ -> false
    | LstMem (sel, _, _) -> has_ptr ptr sel
    | LstPtr (sel, p) -> if ptr = p then true else has_ptr ptr sel
end

(** VarとNamed.Varの対応 *)
module NVarEnv = struct
  type t = (Var.t * (Named.Var.t * kind)) list
  and kind =
    | Cell
    | Ptr
    | Lst of lst
  and lst = {
    length: int option;
    mem: t;
  }

  let to_list (t: t): (Var.t * (Named.Var.t * kind)) list = t

  let lookup (v: Var.t) (t: t) =
    match List.assoc_opt v t with
    | Some x -> x
    | None -> failwith @@ sprintf "not found: %s" v

  (** [gen_using_field dfn parent field]
      [parent]は[field]を展開する位置を[Named.Var.t]のlistで表す *)
  let rec gen_using_field (dfn: Named.Dfn.t) (parent: Named.Var.t list) (field: Field.t)
      : Named.Dfn.t * t =
    List.fold_left_map (fun dfn (var, f_kind) ->
      let nvar = Named.Var.gen_named var in
      let key = parent @ [ nvar ] in
      match f_kind with
      | Field.Cell ->
          let dfn = Named.Dfn.extend key Named.Dfn.Cell dfn in
          let env_elm = (var, (nvar, Cell)) in
          (dfn, env_elm)
      | Field.Ptr ->
          let dfn = Named.Dfn.extend key Named.Dfn.Ptr dfn in
          let env_elm = (var, (nvar, Ptr)) in
          (dfn, env_elm)
      | Field.Lst { length; mem; } ->
          let nlst = Named.Dfn.Lst { length; mem = Named.Dfn.empty; } in
          let dfn = Named.Dfn.extend key nlst dfn in
          let dfn, mem = gen_using_field dfn key mem in
          let env_elm = (var, (nvar, Lst { length; mem })) in
          (dfn, env_elm)
    ) dfn field

end


type va_env = (Var.t * value) list
and value =
  | VaInt of int
  | VaBool of bool
  | VaSel of Sel.t * NVarEnv.t option
  | VaFun of va_env * Var.t * expr
  | VaBlock of va_env * stmt list

module VaEnv = struct
  type t = va_env

  let empty: t = []

  let extend (var: Var.t) (value: value) (t: t): t =
    (var, value) :: t

  let extend_with_nvar_env (diving: Sel.t option) (nvar_env: NVarEnv.t) (t: t): t =
    NVarEnv.to_list nvar_env |>
      List.fold_left (fun t (var, (nvar, kind)) ->
        match kind with
        | NVarEnv.Cell ->
            let sel = Sel.base_or_mem diving nvar in
            (var, VaSel (sel, None)) :: t
        | Ptr ->
            assert false
        | Lst { mem=nvar_env_lst; _ } ->
            let sel = Sel.base_or_mem diving nvar in
            (var, VaSel (sel, Some nvar_env_lst)) :: t
      ) t

  let lookup (var: Var.t) (t: t) =
    match List.assoc_opt var t with
    | Some v -> v
    | None -> failwith @@ sprintf "not found: %s" var
end

module Value = struct
  type t = value

  let to_int = function
    | VaInt i -> i
    | _ -> failwith "value is not a integer"
  let to_bool = function
    | VaBool b -> b
    | _ -> failwith "value is not a boolean"
  let to_sel_and_nvar_env = function
    | VaSel (sel, nvar_env) -> (sel, nvar_env)
    | _ -> failwith "value is not a selector"
  let to_nsel_or_nptr v =
    let sel, _ = to_sel_and_nvar_env v in
    Sel.to_nsel_or_nptr sel
  let to_nsel v =
    let sel, _ = to_sel_and_nvar_env v in
    Sel.to_nsel sel
  let to_fun = function
    | VaFun (env, v, e) -> (env, v, e)
    | _ -> failwith "value is not a function"
  let to_block = function
    | VaBlock (env, block) -> (env, block)
    | _ -> failwith "value is not a block"

  let equal x y =
    match x, y with
    | VaInt x, VaInt y -> x = y
    | VaBool x, VaBool y -> x = y
    | VaSel (x, _), VaSel (y, _) -> x = y
    | _ -> failwith "type error: ="
end


module Program = struct
  type t = Field.t * Stmt.t list
end


module Codegen = struct
  type envs = {
    va_env: VaEnv.t;
    diving: Sel.t option;
    diving_vars: (Sel.t * NVarEnv.t) list
  }

  let rec eval (envs: envs) (expr: Expr.t) =
    let open Value in
    let { va_env = env; _ } = envs in
    match expr with
    | ExVar v -> VaEnv.lookup v env
    | ExInt i -> VaInt i
    | ExBool b -> VaBool b
    | ExSelMem (ex_parent, ex_index_opt, var) -> begin
        let index =
          match ex_index_opt with
          | None -> 0
          | Some ex_index -> eval envs ex_index |> to_int
        in
        let parent = eval envs ex_parent |> to_sel_and_nvar_env in
        match parent with
        | _, None -> failwith "parent is not selecting a list"
        | sel, Some nvar_env -> begin
            let nvar, nvar_kind = NVarEnv.lookup var nvar_env in
            let sel = Sel.LstMem (sel, index, nvar) in
            let nvar_env_opt =
                match nvar_kind with
                | NVarEnv.Cell -> None
                | Ptr -> failwith "child is a pointer"
                | Lst { mem=nvar_env; _ } -> Some nvar_env
            in
            VaSel (sel, nvar_env_opt)
          end
      end
    | ExSelPtr (ex_parent, var) -> begin
        let parent = eval envs ex_parent |> to_sel_and_nvar_env in
        match parent with
        | _, None -> failwith "parent is not selecting a list"
        | Sel.LstPtr _, _ -> failwith "parent is selecting a pointer"
        | sel, Some nvar_env -> begin
            let nvar, nvar_kind = NVarEnv.lookup var nvar_env in
            match nvar_kind with
            | Cell | Lst _ -> failwith "child is not a pointer"
            | Ptr ->
                let sel = Sel.LstPtr (sel, nvar) in
                VaSel (sel, Some nvar_env)
          end
      end
    | ExFun (var, ex) -> VaFun (env, var, ex)
    | ExApp (ex_fn, ex_arg) ->
        let env_fun, var_arg, ex_fun = eval envs ex_fn |> to_fun in
        let arg = eval envs ex_arg in
        let env_fun = VaEnv.extend var_arg arg env_fun in
        eval { envs with va_env = env_fun } ex_fun
    | ExBlock st_list -> VaBlock (env, st_list)
    | ExBOpInt (ex_left, bop, ex_right) -> begin
        let left = eval envs ex_left |> to_int in
        let right = eval envs ex_right |> to_int in
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
        let i = eval envs ex_int |> to_int in
        VaInt (-i)
    | ExEqual (ex_left, ex_right) ->
        let left = eval envs ex_left in
        let right = eval envs ex_right in
        VaBool (Value.equal left right)
    | ExIf (ex_cond, ex_then, ex_else) ->
        let cond = eval envs ex_cond |> to_bool in
        eval envs (if cond then ex_then else ex_else)
    | ExLet (var, ex_var, ex_child) ->
        let value_var = eval envs ex_var in
        let va_env = VaEnv.extend var value_var envs.va_env in
        eval { envs with va_env; } ex_child

  let codegen (program: Program.t) =
    let field, st_list = program in
    let dfn, nvar_env = NVarEnv.gen_using_field Named.Dfn.empty [] field in
    let va_env = VaEnv.extend_with_nvar_env None nvar_env VaEnv.empty in
    let rec codegen envs states st_list =
      let { va_env; diving; diving_vars } = envs in
      let states, code_list =
        List.fold_left_map (fun dfn stmt ->
          match stmt with
          | StAdd (sign, ex_sel, ex_i_opt) -> begin
              let nsel = eval envs ex_sel |> Value.to_nsel in
              let i =
                match ex_i_opt with
                | None -> 1
                | Some ex_i -> eval envs ex_i |>  Value.to_int
              in
              let code = [ Named.Cmd.Add (i * sign, nsel) ] in
              (dfn, code)
            end
          | StPut ex_sel ->
              let nsel = eval envs ex_sel |> Value.to_nsel in
              let code = [ Named.Cmd.Put nsel ] in
              (dfn, code)
          | StGet ex_sel ->
              let nsel = eval envs ex_sel |> Value.to_nsel in
              let code = [ Named.Cmd.Get nsel ] in
              (dfn, code)
          | StWhile (ex_sel, st_list) -> begin
              match eval envs ex_sel |> Value.to_nsel_or_nptr with
              | Sel.NSel nsel ->
                  let states, code_loop = codegen envs dfn st_list in
                  let code = [ Named.Cmd.Loop (nsel, code_loop) ] in
                  (states, code)
              | Sel.NPtr (nsel, ptr) ->
                  let states, code_loop = codegen envs dfn st_list in
                  let code = [ Named.Cmd.LoopPtr (nsel, ptr, code_loop) ] in
                  (states, code)
            end
          | StIf (ex_sel, st_list_then, st_list_else) ->
              let nsel = eval envs ex_sel |> Value.to_nsel in
              let dfn_key = Named.Sel.to_dfn_key nsel in
              let dfn =
                match Named.Dfn.lookup dfn_key dfn with
                | None -> failwith @@ sprintf "not found: %s" (Named.Sel.pretty nsel)
                | Some Named.Dfn.CellIfable -> dfn
                | Some Cell ->  Named.Dfn.extend dfn_key Named.Dfn.CellIfable dfn
                | Some (Lst _ | Ptr) -> failwith "condition selector is not selecting a cell"
              in
              let dfn, code_then = codegen envs dfn st_list_then in
              let dfn, code_else = match st_list_else with
                | None -> (dfn, [])
                | Some st_list_else -> codegen envs dfn st_list_else
              in
              let code = [ Named.Cmd.If (nsel, code_then, code_else) ] in
              (dfn, code)
          | StShift (sign, ex_ptr, ex_i_opt) ->
              let sel, _ = eval envs ex_ptr |> Value.to_sel_and_nvar_env in
              let nsel, ptr = Sel.to_nptr sel in
              let i = sign * match ex_i_opt with
                | None -> 1
                | Some ex_i -> eval envs ex_i |> Value.to_int
              in
              let code_move_var = diving_vars |>
                List.filter_map (fun (sel_diving, nvar_env) ->
                  if sel = sel_diving then (* 変数をコピーする *)
                    NVarEnv.to_list nvar_env |>
                    List.map (fun (_, (nvar, kind)) ->
                      match kind with
                      | NVarEnv.Lst _ | Ptr -> failwith "not implemented (shift local list)"
                      | NVarEnv.Cell ->
                          let nsel_origin = Sel.LstMem (sel, 0, nvar) |> Sel.to_nsel in
                          let nsel_dest = Sel.LstMem (sel, i, nvar) |> Sel.to_nsel in
                          [ Named.Cmd.Loop (nsel_origin,
                            [ Add (-1, nsel_origin);
                              Add (1, nsel_dest); ]) ]
                    ) |> List.flatten |> Option.some
                  else if Sel.has_ptr ptr sel_diving then
                      failwith "shift is prohibited because interfering local variables exist"
                  else None
                ) |> List.flatten
              in
              let code_shift = [ Named.Cmd.Shift (i, nsel, ptr) ] in
              (dfn, code_move_var @ code_shift)
          | StVar (field, st_list) ->
              let dfn_key_diving = match diving with
                | None -> []
                | Some sel -> Sel.to_dfn_key sel
              in
              let dfn, nvar_env = NVarEnv.gen_using_field dfn dfn_key_diving field in
              let va_env = VaEnv.extend_with_nvar_env diving nvar_env va_env in
              let diving_vars = match diving with
                | None -> diving_vars
                | Some sel -> (sel, nvar_env) :: diving_vars
              in
              let envs = { envs with va_env; diving_vars; } in
              let dfn, code_child = codegen envs dfn st_list in
              (* 変数が使用したセルをゼロにする *)
              let code_clean =
                NVarEnv.to_list nvar_env |>
                List.map (fun (_, (nvar, kind)) ->
                  match kind with
                  | NVarEnv.Lst _ | Ptr -> failwith "not implemented (local list)"
                  | NVarEnv.Cell ->
                      let sel = Sel.base_or_mem diving nvar in
                      let nsel = Sel.to_nsel sel in
                      [ Named.Cmd.Loop (nsel, [ Add (-1, nsel) ]) ]
                ) |> List.flatten
              in
              (dfn, code_child @ code_clean)
          | StLet (var, ex, st_list) ->
              let value = eval envs ex in
              let va_env = VaEnv.extend var value va_env in
              codegen { envs with va_env } dfn st_list
          | StExpand ex_block ->
              let va_env, st_list = eval envs ex_block |> Value.to_block in
              codegen { envs with va_env } dfn st_list
          | StDive (ex_ptr, st_list) ->
              let sel, _ = eval envs ex_ptr |> Value.to_sel_and_nvar_env in
              codegen { envs with diving = Some sel } dfn st_list
        ) states st_list
      in
      (states, List.flatten code_list)
    in
    codegen { va_env; diving = None; diving_vars = [] } dfn st_list
end