open Support.Error
open Syntax

type ctx =
  { envs: Eval.envs;
    diving: Ir.Sel.index option;
      (* alloc文がフィールドを確保する位置 *)
    diving_fields: (Ir.Sel.index * IrIdEnv.t) list
      (* インデックスとその下に確保されたフィールドの対応 *)
  }

let generate (envs : Eval.envs) (stmts: top_gen) : Ir.Field.main * unit Ir.Code.t =
  let open Eval.Value in
  let nmain = Ir.Field.empty_main () in
  let ctx = { envs; diving=None; diving_fields=[] } in
  let rec gen (ctx: ctx) (st_list: stmt list): unit * unit Ir.Code.t =
    let { envs; diving; diving_fields } = ctx in
    let (), code_list =
      List.fold_left_map (fun () stmt ->
        let { i = info; v = stmt } = stmt in
        match stmt with
        | StAdd (sign, ex_sel, ex_i_opt) -> begin
            (* nsel というのは Ir.Sel の旧名の Named.Sel のこと *)
            let nsel = Eval.eval envs ex_sel |> to_cell ex_sel.i in
            let i =
              match ex_i_opt with
              | None -> 1
              | Some ex_i -> Eval.eval envs ex_i |>  to_int ex_i.i
            in
            let code = Ir.Code.from_list [ Add (i * sign, nsel) ] in
            ((), code)
          end
        | StPut ex_sel ->
            let nsel = Eval.eval envs ex_sel |> to_cell ex_sel.i in
            let code = Ir.Code.from_list [ Ir.Code.Put nsel ] in
            ((), code)
        | StGet ex_sel ->
            let nsel = Eval.eval envs ex_sel |> to_cell ex_sel.i in
            let code = Ir.Code.from_list [ Get nsel ] in
            ((), code)
        | StWhile (sel_ex, st_list) -> begin
            let sel = Eval.eval envs sel_ex |> to_cell sel_ex.i in
            let (), child_code = gen ctx st_list in
            let code =
              Ir.Code.from_list [ Loop (sel, child_code) ]
            in
            ((), code)
          end
        | StILoop (index_ex, st_list) ->
            let index = Eval.eval envs index_ex |> to_index index_ex.i in
            let (), child_code = gen ctx st_list in
            let code =
              Ir.Code.from_list [ ILoop (index, child_code) ]
            in
            ((), code)
        | StIf (ex_sel, st_list_then, st_list_else) ->
            let nsel = Eval.eval envs ex_sel |> to_cell ex_sel.i in
            let nmtype = Ir.Sel.find_mtype nmain nsel in
            (match nmtype with
            | Ir.Field.Cell cell ->  cell.ifable <- true
            | _ -> assert false
                (* TODO: to_cellの時点で弾かれて到達しないはず。一応試す *)
            );
            let (), code_then = gen ctx st_list_then in
            let (), code_else = match st_list_else with
              | None -> ((), [])
              | Some st_list_else -> gen ctx st_list_else
            in
            let code =
              Ir.Code.from_list [ If (nsel, code_then, code_else) ] in
            ((), code)
        | StShift (sign, ex_idx, ex_i_opt) ->
            let index = Eval.eval envs ex_idx |> to_index ex_idx.i in
            let i = sign * match ex_i_opt with
              | None -> 1
              | Some ex_i -> Eval.eval envs ex_i |> to_int ex_i.i
            in
            if abs i <> 1 then
              error_at info "2 or more shift is not implemented";
            (* シフト時に移動させられるdiving下の一時セルのid *)
            let followers =
              diving_fields
              |> List.filter_map
                (fun (diving, irid_env) ->
                  if index = diving then (* セルの中身をコピーする *)
                    IrIdEnv.to_list irid_env |>
                    List.map
                      (fun (_, { v=(id, mtype); i=_ }) ->
                        match mtype with
                        | IrIdEnv.Cell -> id
                        | IrIdEnv.Array _ | Index -> assert false
                            (* allocを試みた時点で弾かれるので到達できない *)
                      )
                    |> Option.some
                  else if Ir.Sel.is_via_index index (fst diving) then
                    error_at info "Shift is prohibited because an allocated field (under $dive) interfere"
                  else None
                )
              |> List.flatten
            in
            let code_shift =
              Ir.Code.from_list [ Shift { n=i; index; followers } ]
            in
            ((), code_shift)
        | StBuild (field, st_list) ->
            let irid_env = IrIdEnv.gen_using_field nmain nmain.finite field in
            let va_env = extend_env_with_irid_env None irid_env envs.va_env in
            let ctx = { ctx with envs={ envs with va_env } } in
            let (), child_code = gen ctx st_list in
            ((), child_code)
        | StAlloc (field, st_list) ->
            let irfield = match diving with
              | None -> nmain.finite
              | Some (arr_sel, _) -> begin
                  match Ir.Sel.find_mtype nmain arr_sel with
                  | Ir.Field.Array { members; _ } -> members
                  | Cell _ | Index -> assert false
                      (* dive文の処理でインデックス以外がdivingに乗ることは弾かれる *)
                end
            in
            let irid_env = IrIdEnv.gen_using_field nmain irfield field in
            let va_env = extend_env_with_irid_env diving irid_env envs.va_env in
            let envs = { envs with va_env } in
            let diving_fields = match diving with
              | None -> diving_fields
              | Some sel -> (sel, irid_env) :: diving_fields
            in
            let ctx = { ctx with envs; diving_fields; } in
            let (), code_child = gen ctx st_list in
            (* 確保するセルに対するゼロ初期化 *)
            let code_clean =
              IrIdEnv.to_list irid_env |>
              List.map (fun (_, { v=(id, mtype); i }) ->
                match mtype with
                | IrIdEnv.Array _ -> error_at i "Allocating temporary arrays is prohibited"
                | IrIdEnv.Index -> assert false
                    (* トップレベルにindexの宣言を試みたらgen_using_fieldがエラーを発生させるはず *)
                | IrIdEnv.Cell ->
                    let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
                    Ir.Code.from_list [ Reset sel ]
              ) |> List.flatten
            in
            ((), code_clean @ code_child @ code_clean)
        | StLet (binding, st_list) ->
            let envs = Eval.eval_let_binding ~export:false ctx.envs binding in
            gen { ctx with envs } st_list
        | StExpand ex_block ->
            let envs, st_list = Eval.eval envs ex_block |> to_block ex_block.i in
            gen { ctx with envs } st_list
        | StDive (index_ex, st_list) ->
            let index = Eval.eval envs index_ex |> to_index index_ex.i in
            gen { ctx with diving = Some index } st_list
      ) () st_list
    in
    ((), List.flatten code_list)
  in
  let (), cmd_list = gen ctx stmts in
  (nmain, cmd_list)