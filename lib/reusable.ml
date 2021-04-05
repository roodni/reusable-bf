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
and expr =
  | ExVar of Var.t
  | ExInt of int
  | ExSelMem of expr * expr option * Var.t
  | ExSelPtr of expr * Var.t
  | ExFun of Var.t * expr
  | ExApp of expr * expr
  | ExBlock of stmt list

module Stmt = struct
  type t = stmt
end

module Expr = struct
  type t = expr
end


(* Named.Selをラップする *)
module Sel = struct
  type t =
    | Base of Named.Var.t
    | LstMem of t * int * Named.Var.t
    | LstPtr of t * Named.Var.t
  
  type nsel_or_nptr =
    | NSel of Named.Sel.t
    | NPtr of Named.Sel.t * Named.Var.t
  
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
end

(* VarとNamed.Varの対応 *)
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
  
  let rec gen_using_field (dfn: Named.Dfn.t) (field: Field.t): Named.Dfn.t * t =
    List.fold_left_map (fun dfn (var, f_kind) ->
      let nvar = Named.Var.gen_named var in
      match f_kind with
      | Field.Cell ->
          let dfn = Named.Dfn.extend [ nvar ] Named.Dfn.Cell dfn in
          let env_elm = (var, (nvar, Cell)) in
          (dfn, env_elm)
      | Field.Ptr ->
          let dfn = Named.Dfn.extend [ nvar ] Named.Dfn.Ptr dfn in
          let env_elm = (var, (nvar, Ptr)) in
          (dfn, env_elm)
      | Field.Lst { length; mem; } ->
          let dfn_mem, env = gen_using_field Named.Dfn.empty mem in
          let dfn =
            Named.Dfn.extend [ nvar ] (Named.Dfn.Lst { length; mem=dfn_mem; }) dfn
          in
          let env_elm = (var, (nvar, Lst { length; mem=env; })) in
          (dfn, env_elm)
    ) dfn field
  
  let codegen_clean (t: t) =
    t |> List.map (fun (_, (nv, kind)) ->
      let nsel = Named.Sel.V nv in
      match kind with
      | Cell -> [ Named.Cmd.Loop (nsel, [ Named.Cmd.Add (-1, nsel) ]) ]
      | Ptr | Lst _ -> failwith "not implemented"
    ) |> List.flatten
  
end


type va_env = (Var.t * value) list
and value =
  | VaInt of int
  | VaSel of Sel.t * NVarEnv.t option
  | VaFun of va_env * Var.t * expr
  | VaBlock of va_env * stmt list

module VaEnv = struct
  type t = va_env

  let empty: t = []

  let extend (var: Var.t) (value: value) (t: t): t =
    (var, value) :: t

  let extend_with_nvar_env (nvar_env: NVarEnv.t) (t: t): t =
    NVarEnv.to_list nvar_env |>
      List.fold_left (fun t (var, (nvar, kind)) ->
        match kind with
        | NVarEnv.Cell ->
            let sel = Sel.Base nvar in
            (var, VaSel (sel, None)) :: t
        | Ptr ->
            assert false
        | Lst { mem=nvar_env_lst; _ } ->
            let sel = Sel.Base nvar in
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
    | _ -> failwith "value is not int"
  let to_sel_and_nvar_env = function
    | VaSel (sel, nvar_env) -> (sel, nvar_env)
    | _ -> failwith "value is not selector"
  let to_nsel_or_nptr v =
    let sel, _ = to_sel_and_nvar_env v in
    Sel.to_nsel_or_nptr sel
  let to_nsel v =
    match to_nsel_or_nptr v with
    | Sel.NSel nsel -> nsel
    | Sel.NPtr _ -> failwith "selector is selecting pointer"
  let to_nptr v =
    match to_nsel_or_nptr v with
    | Sel.NSel _ -> failwith "selector is not selecting pointer"
    | Sel.NPtr (nsel, ptr) -> (nsel, ptr)
  let to_fun = function
    | VaFun (env, v, e) -> (env, v, e)
    | _ -> failwith "value is not function"
  let to_block = function
    | VaBlock (env, block) -> (env, block)
    | _ -> failwith "value is not block"

  let rec eval (env: VaEnv.t) = function
    | ExVar v -> VaEnv.lookup v env
    | ExInt i -> VaInt i
    | ExSelMem (ex_parent, ex_index_opt, var) -> begin
        let index =
          match ex_index_opt with
          | None -> 0
          | Some ex_index -> eval env ex_index |> to_int
        in
        let parent = eval env ex_parent |> to_sel_and_nvar_env in
        match parent with
        | _, None -> failwith "parent is not selecting list"
        | sel, Some nvar_env -> begin
            let nvar, nvar_kind = NVarEnv.lookup var nvar_env in
            let sel = Sel.LstMem (sel, index, nvar) in
            let nvar_env_opt =
                match nvar_kind with
                | NVarEnv.Cell -> None
                | Ptr -> failwith "pointer is selected"
                | Lst { mem=nvar_env; _ } -> Some nvar_env
            in
            VaSel (sel, nvar_env_opt)
          end
      end
    | ExSelPtr (ex_parent, var) -> begin
        let parent = eval env ex_parent |> to_sel_and_nvar_env in
        match parent with
        | _, None -> failwith "parent is not selecting list"
        | Sel.LstPtr _, _ -> failwith "parent is selecting pointer"
        | sel, Some nvar_env -> begin
            let nvar, nvar_kind = NVarEnv.lookup var nvar_env in
            match nvar_kind with
            | Cell | Lst _ -> failwith "pointer is not selected"
            | Ptr ->
                let sel = Sel.LstPtr (sel, nvar) in
                VaSel (sel, Some nvar_env)
          end
      end
    | ExFun (var, ex) -> VaFun (env, var, ex)
    | ExApp (ex_fn, ex_arg) ->
        let env_fun, var_arg, ex_fun = eval env ex_fn |> to_fun in
        let arg = eval env ex_arg in
        let env_fun = VaEnv.extend var_arg arg env_fun in
        eval env_fun ex_fun
    | ExBlock st_list -> VaBlock (env, st_list)

end


module Program = struct
  type t = Field.t * Stmt.t list

  let codegen (program: t) =
    let field, st_list = program in
    let dfn, nvar_env = NVarEnv.gen_using_field Named.Dfn.empty field in
    let va_env = VaEnv.extend_with_nvar_env nvar_env VaEnv.empty in
    let rec codegen va_env states st_list =
      let states, code_list =
        List.fold_left_map (fun dfn stmt ->
          match stmt with
          | StAdd (sign, ex_sel, ex_i_opt) -> begin
              let nsel = Value.eval va_env ex_sel |> Value.to_nsel in
              let i =
                match ex_i_opt with
                | None -> 1
                | Some ex_i -> Value.eval va_env ex_i |>  Value.to_int
              in
              let code = [ Named.Cmd.Add (i * sign, nsel) ] in
              (dfn, code)
            end
          | StPut ex_sel ->
              let nsel = Value.eval va_env ex_sel |> Value.to_nsel in
              let code = [ Named.Cmd.Put nsel ] in
              (dfn, code)
          | StGet ex_sel ->
              let nsel = Value.eval va_env ex_sel |> Value.to_nsel in
              let code = [ Named.Cmd.Get nsel ] in
              (dfn, code)
          | StWhile (ex_sel, st_list) -> begin
              match Value.eval va_env ex_sel |> Value.to_nsel_or_nptr with
              | Sel.NSel nsel ->
                  let envs, code_loop = codegen va_env dfn st_list in
                  let code = [ Named.Cmd.Loop (nsel, code_loop) ] in
                  (envs, code)
              | Sel.NPtr (nsel, ptr) ->
                  let envs, code_loop = codegen va_env dfn st_list in
                  let code = [ Named.Cmd.LoopPtr (nsel, ptr, code_loop) ] in
                  (envs, code)
            end
          | StIf (ex_sel, st_list_then, st_list_else) ->
              let nsel = Value.eval va_env ex_sel |> Value.to_nsel in
              let dfn_key = Named.Sel.to_dfn_key nsel in
              let dfn =
                match Named.Dfn.lookup dfn_key dfn with
                | Named.Dfn.CellIfable -> dfn
                | Cell ->  Named.Dfn.extend dfn_key Named.Dfn.CellIfable dfn
                | Lst _ | Ptr -> failwith "condition selector is not selecting cell"
              in
              let dfn, code_then = codegen va_env dfn st_list_then in
              let dfn, code_else = match st_list_else with
                | None -> (dfn, [])
                | Some st_list_else -> codegen va_env dfn st_list_else
              in
              let code = [ Named.Cmd.If (nsel, code_then, code_else) ] in
              (dfn, code)
          | StShift (sign, ex_ptr, ex_i_opt) ->
              let nsel, ptr = Value.eval va_env ex_ptr |> Value.to_nptr in
              let i = match ex_i_opt with
                | None -> 1
                | Some ex_i -> Value.eval va_env ex_i |> Value.to_int
              in
              let code = [ Named.Cmd.Shift (sign * i, nsel, ptr) ] in
              (dfn, code)
          | StVar (field, st_list) ->
              let dfn, nvar_env = NVarEnv.gen_using_field dfn field in
              let va_env = VaEnv.extend_with_nvar_env nvar_env va_env in
              let dfn, code_child = codegen va_env dfn st_list in
              let code_clean = NVarEnv.codegen_clean nvar_env in
              (dfn, code_child @ code_clean)
          | StLet (var, ex, st_list) ->
              let value = Value.eval va_env ex in
              let va_env = VaEnv.extend var value va_env in
              codegen va_env dfn st_list
          | StExpand ex_block ->
              let va_env, st_list = Value.eval va_env ex_block |> Value.to_block in
              codegen va_env dfn st_list
        ) states st_list
      in
      (states, List.flatten code_list)
    in
    codegen va_env dfn st_list
end