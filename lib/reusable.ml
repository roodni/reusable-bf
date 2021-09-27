open Batteries
open Printf
open Support.Error

module Var = struct
  type t = string
end


module Field = struct
  type t = (Var.t * mtype) withinfo list
  and mtype =
    | Cell
    | Index
    | Array of {
        length : int option;
        mem : t;
      }
end

module BOpInt = struct
  type t = Add | Sub | Mul | Div | Mod | Lt | Leq
end

type pat = pat' withinfo
and pat' =
  | PatVar of Var.t
  | PatWild
  | PatCons of pat * pat
  | PatNil
  | PatPair of pat * pat
  | PatInt of int
  | PatBool of bool

type stmt = stmt' withinfo
and stmt' =
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
and expr = expr' withinfo
and expr' =
  | ExVar of Var.t
  | ExInt of int
  | ExBool of bool
  | ExStr of char list
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
  | ExNil
  | ExCons of expr * expr
  | ExMatch of expr * (pat * expr) list
  | ExPair of expr * expr


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

  (* to_nsel や to_nptr は例外を発生させるべきではなくて、infoも必要ない
      Namedのリファクタリングに合わせて取り除く *)
  let to_nsel info sel =
    match to_nsel_or_nptr sel with
    | NSel nsel -> nsel
    | NPtr _ -> error_at info "selector(cell) or selector(array) expected"
  let to_nptr info sel =
    match to_nsel_or_nptr sel with
    | NSel _ -> error_at info "selector(index) expected"
    | NPtr (nsel, ptr) -> (nsel, ptr)

  (** ポインタ[ptr]をセレクタ[sel]が経由するかどうか
      [has_ptr ptr sel] *)
  let rec has_ptr ptr sel =
    match sel with
    | Base _ -> false
    | LstMem (sel, _, _) -> has_ptr ptr sel
    | LstPtr (sel, p) -> if ptr = p then true else has_ptr ptr sel
end

(** VarとNamed.Varの対応 *)
module NVarEnv : sig
  type t
  and binded = (Named.Var.t * mtype) withinfo
  and mtype =
    | Cell
    | Index
    | Array of {
        length: int option;
        mem: t;
      }

  val to_list : t -> (Var.t * binded) list
  val lookup : Var.t -> t -> binded option

  (** [gen_using_field dfn parent field]
      [parent]は[field]を展開する位置を[Named.Var.t]のlistで表す *)
  val gen_using_field : Named.Dfn.t -> Named.Var.t list -> Field.t -> Named.Dfn.t * t

end = struct
  type t = (Var.t * binded) list
  and binded = (Named.Var.t * mtype) withinfo
  and mtype =
    | Cell
    | Index
    | Array of {
        length: int option;
        mem: t;
      }

  let to_list (t: t) = t
  let lookup (v: Var.t) (t: t) = List.assoc_opt v t

  let rec gen_using_field (dfn: Named.Dfn.t) (parent: Named.Var.t list) (field: Field.t)
      : Named.Dfn.t * t =
    List.fold_left_map (fun dfn field_mem ->
      let var, mtype = field_mem.v in
      let nvar = Named.Var.gen_named var in
      let key = parent @ [ nvar ] in
      match mtype with
      | Field.Cell ->
          let dfn = Named.Dfn.extend key Named.Dfn.Cell dfn in
          let env_elm = (var, withinfo field_mem.i (nvar, Cell)) in
          (dfn, env_elm)
      | Field.Index ->
          let dfn = Named.Dfn.extend key Named.Dfn.Ptr dfn in
          let env_elm = (var, withinfo field_mem.i (nvar, Index)) in
          (dfn, env_elm)
      | Field.Array { length; mem; } ->
          let nlst = Named.Dfn.Lst { length; mem = Named.Dfn.empty; } in
          let dfn = Named.Dfn.extend key nlst dfn in
          let dfn, mem = gen_using_field dfn key mem in
          let env_elm = (var, withinfo field_mem.i (nvar, Array { length; mem })) in
          (dfn, env_elm)
    ) dfn field

end


module Builtin = struct
  type t = unit

  let of_var (v: Var.t) =
    match v with
    | _ -> None
end

module Value : sig
  type t =
    | VaInt of int
    | VaBool of bool
    | VaSel of Sel.t * NVarEnv.t option
    | VaFun of env * Var.t * expr
    | VaBuiltin of Builtin.t
    | VaBlock of env * stmt list
    | VaList of t list
    | VaPair of t * t
  and env

  val to_int : info -> t -> int
  val to_bool : info -> t -> bool
  val to_block : info -> t -> env * stmt list
  val to_list : info -> t -> t list
  val to_pair : info -> t -> t * t
  val to_sel_and_nvar_env : info -> t -> Sel.t * NVarEnv.t option
  val to_nsel_or_nptr : info -> t -> Sel.nsel_or_nptr
  val to_nsel : info -> t -> Named.Sel.t

  val equal : t -> t -> bool option

  val env_empty : env
  val env_extend : Var.t -> t -> env -> env
  val env_lookup : Var.t -> env -> t option

  (** [env_extend_with_nvar_env diving nvar_env env] *)
  val env_extend_with_nvar_env : Sel.t option -> NVarEnv.t -> env -> env

end = struct
  type t =
    | VaInt of int
    | VaBool of bool
    | VaSel of Sel.t * NVarEnv.t option
    | VaFun of env * Var.t * expr
    | VaBuiltin of Builtin.t
    | VaBlock of env * stmt list
    | VaList of t list
    | VaPair of t * t
  and env = (Var.t * t) list

  let to_int info = function
    | VaInt i -> i
    | _ -> error_at info "int expected"
  let to_bool info = function
    | VaBool b -> b
    | _ -> error_at info "bool expected"
  let to_block info = function
    | VaBlock (env, block) -> (env, block)
    | _ -> error_at info "block expected"
  let to_list info = function
    | VaList l -> l
    | _ -> error_at info "list expected"
  let to_pair info = function
    | VaPair (v1, v2) -> (v1, v2)
    | _ -> error_at info "pair expected"

  (* セレクタまわりは修正した方が良い *)
  let to_sel_and_nvar_env info = function
    | VaSel (sel, nvar_env) -> (sel, nvar_env)
    | _ -> error_at info "Must be a selector"
  let to_nsel_or_nptr info v =
    let sel, _ = to_sel_and_nvar_env info v in
    Sel.to_nsel_or_nptr sel
  let to_nsel info v =
    let sel, _ = to_sel_and_nvar_env info v in
    Sel.to_nsel info sel

  let equal x y =
    let rec equal x y =
      match x, y with
      | VaInt x, VaInt y -> x = y
      | VaBool x, VaBool y -> x = y
      | VaSel (x, _), VaSel (y, _) -> x = y
      | VaList x, VaList y -> begin
          try List.for_all2 equal x y with
          | Invalid_argument _ -> false
        end
      | VaPair (x1, x2), VaPair (y1, y2) -> equal x1 y1 && equal x2 y2
      | _ -> raise Exit
    in
    try Some (equal x y) with
    | Exit -> None

  let env_empty: env = []

  let env_extend (var: Var.t) (value: t) (env: env): env =
    (var, value) :: env

  let env_extend_with_nvar_env (diving: Sel.t option) (nvar_env: NVarEnv.t) (env: env) =
    List.fold_left
      (fun t (var, { v = (nvar, mtype); i }) ->
        match mtype with
        | NVarEnv.Cell ->
            let sel = Sel.base_or_mem diving nvar in
            (var, VaSel (sel, None)) :: t
        | Index ->
            error_at i "Index must be declared as a member of an array"
        | Array { mem=nvar_env_lst; _ } ->
            let sel = Sel.base_or_mem diving nvar in
            (var, VaSel (sel, Some nvar_env_lst)) :: t)
      env
      (NVarEnv.to_list nvar_env)

  let env_lookup (var: Var.t) (env: env) = List.assoc_opt var env
end


module Program = struct
  type toplevel' =
    | Let of Var.t * expr
  type toplevel = toplevel' withinfo
  type main = Field.t * stmt list
  type t = toplevel list * main
end


module Codegen = struct
  let rec matches va_env pat value =
    let open Value in
    let (let*) = Option.bind in
    match pat.v, value with
    | PatVar var, _ -> Some (Value.env_extend var value va_env)
    | PatWild, _ -> Some va_env
    | PatCons (phd, ptl), VaList (vhd :: vtl) ->
        let* va_env = matches va_env phd vhd in
        matches va_env ptl (VaList vtl)
    | PatNil, VaList [] -> Some va_env
    | PatPair (pf, ps), VaPair (vf, vs) ->
        let* va_env = matches va_env pf vf in
        matches va_env ps vs
    | PatInt pi, VaInt vi when pi = vi -> Some va_env
    | PatBool pb, VaBool vb when pb = vb -> Some va_env
    | _ -> None

  type eval_envs = { va_env : Value.env; }

  let rec eval (envs: eval_envs) (expr: expr) : Value.t =
    let open Value in
    let env = envs.va_env in
    let { i = info; v = expr } = expr in
    match expr with
    | ExVar v -> begin
        match env_lookup v env with
        | Some va -> va
        | None -> begin
            match Builtin.of_var v with
            | Some b -> VaBuiltin b
            | None -> error_at info @@ sprintf "Unbound value '%s'" v
          end
      end
    | ExInt i -> VaInt i
    | ExBool b -> VaBool b
    | ExStr cl -> VaList (List.map (fun c -> VaInt (int_of_char c)) cl)
    | ExSelMem (ex_parent, ex_index_opt, var) -> begin
        let index =
          match ex_index_opt with
          | None -> 0
          | Some ex_index -> eval envs ex_index |> to_int ex_index.i
        in
        let parent = eval envs ex_parent |> to_sel_and_nvar_env ex_parent.i in
        match parent with
        | _, None -> error_at ex_parent.i "selector(array) or selector(index) expected"
        | sel, Some nvar_env -> begin
            match NVarEnv.lookup var nvar_env with
            | None -> error_at info @@ sprintf "Unbound member '%s'" var
            | Some { v = (nvar, nvar_mtype); i = _ } ->
                let sel = Sel.LstMem (sel, index, nvar) in
                let nvar_env_opt =
                    match nvar_mtype with
                    | NVarEnv.Cell -> None
                    | Index -> error_at info "Selecting an index (Use '@' instead of ':')"
                    | Array { mem=nvar_env; _ } -> Some nvar_env
                in
                VaSel (sel, nvar_env_opt)
          end
      end
    | ExSelPtr (ex_parent, var) -> begin
        let parent = eval envs ex_parent |> to_sel_and_nvar_env ex_parent.i in
        match parent with
        | _, None | Sel.LstPtr _, _ ->
            error_at ex_parent.i "selector(array) expected"
        | sel, Some nvar_env -> begin
            match NVarEnv.lookup var nvar_env with
            | None -> error_at info @@ sprintf "Unbound member '%s'" var
            | Some { v = (nvar, nvar_mtype); i = _ } -> begin
                match nvar_mtype with
                | Cell | Array _ -> error_at info "Not selecting an index (Use ':' instead of '@')"
                | Index ->
                    let sel = Sel.LstPtr (sel, nvar) in
                    VaSel (sel, Some nvar_env)
              end
          end
      end
    | ExFun (var, ex) -> VaFun (env, var, ex)
    | ExApp (ex_fn, ex_arg) -> begin
        let va_fn = eval envs ex_fn in
        let va_arg = eval envs ex_arg in
        match va_fn with
        | VaFun (env_fun, var_arg, ex_fun) ->
            let env_fun = env_extend var_arg va_arg env_fun in
            eval { va_env = env_fun } ex_fun
        (* | VaBuiltin Fst -> to_pair ex_arg.i va_arg |> fst *)
        | _ -> error_at ex_fn.i "function expected"
      end
    | ExBlock st_list -> VaBlock (env, st_list)
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
        match Value.equal left right with
        | Some b -> VaBool b
        | None -> error_at info "Cannot test equality"
      end
    | ExIf (ex_cond, ex_then, ex_else) ->
        let cond = eval envs ex_cond |> to_bool ex_cond.i in
        eval envs (if cond then ex_then else ex_else)
    | ExLet (var, ex_var, ex_child) ->
        let value_var = eval envs ex_var in
        let va_env = env_extend var value_var envs.va_env in
        eval { va_env } ex_child
    | ExNil -> VaList []
    | ExCons (ex_head, ex_tail) ->
        let head = eval envs ex_head in
        let tail = eval envs ex_tail |> to_list ex_tail.i in
        VaList (head :: tail)
    | ExMatch (ex_matched, pat_ex_list) -> begin
        let va_matched = eval envs ex_matched in
        let env_ex_opt =
          List.find_map_opt
            (fun (pat, ex) ->
              matches envs.va_env pat va_matched
              |> Option.map (fun va_env -> (va_env, ex)))
            pat_ex_list
        in
        match env_ex_opt with
        | Some (va_env, ex) -> eval { va_env } ex
        | None -> error_at info "Match failed"
      end
    | ExPair (ex1, ex2) ->
        let v1 = eval envs ex1 in
        let v2 = eval envs ex2 in
        VaPair (v1, v2)

  let eval_toplevels (envs: eval_envs) (toplevels: Program.toplevel list): eval_envs =
    List.fold_left
      (fun envs toplevel ->
        match toplevel.v with
        | Program.Let (var, expr) ->
            let value = eval envs expr in
            let va_env = Value.env_extend var value envs.va_env in
            { va_env }
      )
      envs toplevels

  type codegen_ctx = {
    eval_envs: eval_envs;
    diving: Sel.t option;
    diving_vars: (Sel.t * NVarEnv.t) list
  }

  let codegen (eval_envs : eval_envs) (main: Program.main) : Named.Dfn.t * Named.Cmd.t list =
    let field, st_list = main in
    let dfn, nvar_env = NVarEnv.gen_using_field Named.Dfn.empty [] field in
    let va_env_main = Value.env_extend_with_nvar_env None nvar_env eval_envs.va_env in
    let eval_envs = { va_env = va_env_main } in
    let ctx = { eval_envs; diving = None; diving_vars = [] } in
    let rec codegen ctx states st_list =
      let { eval_envs; diving; diving_vars } = ctx in
      let states, code_list =
        List.fold_left_map (fun dfn stmt ->
          let { i = info; v = stmt } = stmt in
          match stmt with
          | StAdd (sign, ex_sel, ex_i_opt) -> begin
              let nsel = eval eval_envs ex_sel |> Value.to_nsel ex_sel.i in
              let i =
                match ex_i_opt with
                | None -> 1
                | Some ex_i -> eval eval_envs ex_i |>  Value.to_int ex_i.i
              in
              let code = [ Named.Cmd.Add (i * sign, nsel) ] in
              (dfn, code)
            end
          | StPut ex_sel ->
              let nsel = eval eval_envs ex_sel |> Value.to_nsel ex_sel.i in
              let code = [ Named.Cmd.Put nsel ] in
              (dfn, code)
          | StGet ex_sel ->
              let nsel = eval eval_envs ex_sel |> Value.to_nsel ex_sel.i in
              let code = [ Named.Cmd.Get nsel ] in
              (dfn, code)
          | StWhile (ex_sel, st_list) -> begin
              match eval eval_envs ex_sel |> Value.to_nsel_or_nptr ex_sel.i with
              | Sel.NSel nsel ->
                  let states, code_loop = codegen ctx dfn st_list in
                  let code = [ Named.Cmd.Loop (nsel, code_loop) ] in
                  (states, code)
              | Sel.NPtr (nsel, ptr) ->
                  let states, code_loop = codegen ctx dfn st_list in
                  let code = [ Named.Cmd.LoopPtr (nsel, ptr, code_loop) ] in
                  (states, code)
            end
          | StIf (ex_sel, st_list_then, st_list_else) ->
              let nsel = eval eval_envs ex_sel |> Value.to_nsel ex_sel.i in
              let dfn_key = Named.Sel.to_dfn_key nsel in
              let dfn =
                match Named.Dfn.lookup dfn_key dfn with
                | None -> assert false (* evalが成功するセレクタ式ならば登録されているはず *)
                | Some Named.Dfn.CellIfable -> dfn
                | Some Cell ->  Named.Dfn.extend dfn_key Named.Dfn.CellIfable dfn
                | Some (Lst _ | Ptr) -> error_at ex_sel.i "selector(cell) expected"
              in
              let dfn, code_then = codegen ctx dfn st_list_then in
              let dfn, code_else = match st_list_else with
                | None -> (dfn, [])
                | Some st_list_else -> codegen ctx dfn st_list_else
              in
              let code = [ Named.Cmd.If (nsel, code_then, code_else) ] in
              (dfn, code)
          | StShift (sign, ex_ptr, ex_i_opt) ->
              let sel, _ = eval eval_envs ex_ptr |> Value.to_sel_and_nvar_env ex_ptr.i in
              let nsel, ptr = Sel.to_nptr ex_ptr.i sel in
              let i = sign * match ex_i_opt with
                | None -> 1
                | Some ex_i -> eval eval_envs ex_i |> Value.to_int ex_i.i
              in
              if abs i <> 1 then error_at info "2 or more shift is not implemented";
              let code_move_var = diving_vars |>
                List.filter_map (fun (sel_diving, nvar_env) ->
                  if sel = sel_diving then (* 変数をコピーする *)
                    NVarEnv.to_list nvar_env |>
                    List.map (fun (_, { v = (nvar, mtype); i = _ }) ->
                      match mtype with
                      | NVarEnv.Array _ | Index ->
                          (* varの時点で弾かれるので到達できないはず *)
                          error_at info "Shifting local array is not implemented"
                      | NVarEnv.Cell ->
                          let nsel_origin = Sel.LstMem (sel, 0, nvar) |> Sel.to_nsel unknown_info in
                          let nsel_dest = Sel.LstMem (sel, i, nvar) |> Sel.to_nsel unknown_info in
                          [ Named.Cmd.Loop (nsel_origin,
                            [ Add (-1, nsel_origin);
                              Add (1, nsel_dest); ]) ]
                    ) |> List.flatten |> Option.some
                  else if Sel.has_ptr ptr sel_diving then
                      error_at info "Shift is prohibited because a local cell interferes"
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
              let va_env = Value.env_extend_with_nvar_env diving nvar_env eval_envs.va_env in
              let eval_envs = { va_env } in
              let diving_vars = match diving with
                | None -> diving_vars
                | Some sel -> (sel, nvar_env) :: diving_vars
              in
              let ctx = { ctx with eval_envs; diving_vars; } in
              let dfn, code_child = codegen ctx dfn st_list in
              (* 変数が使用したセルをゼロにする *)
              let code_clean =
                NVarEnv.to_list nvar_env |>
                List.map (fun (_, { v = (nvar, mtype); i }) ->
                  match mtype with
                  | NVarEnv.Array _ -> error_at i "Allocating local arrays is not implemented"
                  | NVarEnv.Index ->
                      assert false
                      (* トップレベルにindexの宣言を試みたらgen_using_fieldがエラーを発生させるはず *)
                  | NVarEnv.Cell ->
                      let sel = Sel.base_or_mem diving nvar in
                      let nsel = Sel.to_nsel unknown_info sel in
                      [ Named.Cmd.Loop (nsel, [ Add (-1, nsel) ]) ]
                ) |> List.flatten
              in
              (dfn, code_child @ code_clean)
          | StLet (var, ex, st_list) ->
              let value = eval eval_envs ex in
              let va_env = Value.env_extend var value eval_envs.va_env in
              let eval_envs = { va_env } in
              codegen { ctx with eval_envs } dfn st_list
          | StExpand ex_block ->
              let va_env, st_list = eval eval_envs ex_block |> Value.to_block ex_block.i in
              let eval_envs = { va_env } in
              codegen { ctx with eval_envs } dfn st_list
          | StDive (ex_ptr, st_list) ->
              let sel, _ = eval eval_envs ex_ptr |> Value.to_sel_and_nvar_env ex_ptr.i in
              let _ = Sel.to_nptr ex_ptr.i sel in
              (* ↑インデックスであることを確認している *)
              codegen { ctx with diving = Some sel } dfn st_list
        ) states st_list
      in
      (states, List.flatten code_list)
    in
    codegen ctx dfn st_list

  let codegen_all (program: Program.t) : Named.Dfn.t * Named.Cmd.t list =
    let toplevels, main = program in
    let envs = eval_toplevels { va_env = Value.env_empty } toplevels in
    codegen envs main


end