open Support.Error
open Syntax

type codegen_ctx = {
  envs: Eval.envs;
  diving: Sel.t option;
  diving_vars: (Sel.t * NVarEnv.t) list
}

let gen_ir_from_main (envs : Eval.envs) (main: main) : Ir.Field.main * unit Ir.Code.t =
  let open Eval.Value in
  let field, st_list = main in
  let nmain = Ir.Field.empty_main () in
  let nvar_env = NVarEnv.gen_using_field nmain nmain.finite field in
  let va_env_main = env_extend_with_nvar_env None nvar_env envs.va_env in
  let envs = { envs with va_env = va_env_main } in
  let ctx = { envs; diving=None; diving_vars=[] } in
  let rec codegen (ctx: codegen_ctx) (st_list: stmt list): unit * unit Ir.Code.t =
    let { envs; diving; diving_vars } = ctx in
    let (), code_list =
      List.fold_left_map (fun () stmt ->
        let { i = info; v = stmt } = stmt in
        match stmt with
        | StAdd (sign, ex_sel, ex_i_opt) -> begin
            let nsel = Eval.eval envs ex_sel |> to_nsel ex_sel.i in
            let i =
              match ex_i_opt with
              | None -> 1
              | Some ex_i -> Eval.eval envs ex_i |>  to_int ex_i.i
            in
            let code =
              Ir.Code.from_list [ Add (i * sign, nsel) ]
            in
            ((), code)
          end
        | StPut ex_sel ->
            let nsel = Eval.eval envs ex_sel |> to_nsel ex_sel.i in
            let code =
              Ir.Code.from_list [ Ir.Code.Put nsel ]
            in
            ((), code)
        | StGet ex_sel ->
            let nsel = Eval.eval envs ex_sel |> to_nsel ex_sel.i in
            let code =
              Ir.Code.from_list [ Get nsel ] in
            ((), code)
        | StWhile (ex_sel, st_list) -> begin
            match Eval.eval envs ex_sel |> to_nsel_or_nptr ex_sel.i with
            | Sel.NSel nsel ->
                let (), code_loop = codegen ctx st_list in
                let code =
                  Ir.Code.from_list
                    [ Loop (nsel, code_loop) ]
                in
                ((), code)
            | Sel.NPtr (nsel, idx) ->
                let (), code_loop = codegen ctx st_list in
                let code =
                  Ir.Code.from_list [ LoopIndex (nsel, idx, code_loop) ]
                in
                ((), code)
          end
        | StIf (ex_sel, st_list_then, st_list_else) ->
            let nsel = Eval.eval envs ex_sel |> to_nsel ex_sel.i in
            let nmtype = Ir.Sel.find_mtype nmain nsel in
            let () =
              match nmtype with
              | Ir.Field.Cell cell ->
                  cell.ifable <- true
              | _ ->
                  (* TODO: Ir.Field.mtype のパターンマッチでエラーを報告するのは良くない。直す *)
                  error_at ex_sel.i "selector(cell) expected"
            in
            let (), code_then = codegen ctx st_list_then in
            let (), code_else = match st_list_else with
              | None -> ((), [])
              | Some st_list_else -> codegen ctx st_list_else
            in
            let code =
              Ir.Code.from_list [ If (nsel, code_then, code_else) ] in
            ((), code)
        | StShift (sign, ex_ptr, ex_i_opt) ->
            let sel, _ = Eval.eval envs ex_ptr |> to_sel_and_nvar_env ex_ptr.i in
            let nsel, ptr = Sel.to_nptr ex_ptr.i sel in
            let i = sign * match ex_i_opt with
              | None -> 1
              | Some ex_i -> Eval.eval envs ex_i |> to_int ex_i.i
            in
            if abs i <> 1 then error_at info "2 or more shift is not implemented";
            (* シフト時に移動させられるdiving中のセルid *)
            let followers =
              diving_vars
              |> List.filter_map
                (fun (sel_diving, nvar_env) ->
                  if sel = sel_diving then (* 変数をコピーする *)
                    NVarEnv.to_list nvar_env |>
                    List.map
                      (fun (_, { v=(nid, mtype); i=_ }) ->
                        match mtype with
                        | NVarEnv.Array _ | Index -> assert false
                            (* allocを試みた時点で弾かれるので到達できない *)
                        | NVarEnv.Cell -> nid
                      )
                    |> Option.some
                  else if Sel.has_ptr ptr sel_diving then
                    error_at info "Shift is prohibited because some allocated cells interfere"
                  else None
                )
              |> List.flatten
            in
            let code_shift =
              Ir.Code.from_list [ Shift { n=i; index=(nsel, ptr); followers } ]
            in
            ((), code_shift)
        | StAlloc (field, st_list) ->
            let nfield = match diving with
              | None -> nmain.finite
              | Some sel ->
                  let nsel, _ =  Sel.to_nptr info sel in
                  let nmtype = Ir.Sel.find_mtype nmain nsel in
                  match nmtype with
                  | Ir.Field.Array { members; _ } -> members
                  | _ -> assert false (* divingに登録されているセレクタは(たぶん)配列を指している *)
            in
            let nvar_env = NVarEnv.gen_using_field nmain nfield field in
            let va_env = env_extend_with_nvar_env diving nvar_env envs.va_env in
            let envs = { envs with va_env } in
            let diving_vars = match diving with
              | None -> diving_vars
              | Some sel -> (sel, nvar_env) :: diving_vars
            in
            let ctx = { ctx with envs; diving_vars; } in
            let (), code_child = codegen ctx st_list in
            (* 確保するセルをゼロで初期化する *)
            let code_clean =
              NVarEnv.to_list nvar_env |>
              List.map (fun (_, { v = (nvar, mtype); i }) ->
                match mtype with
                | NVarEnv.Array _ -> error_at i "Allocating local arrays is prohibited"
                | NVarEnv.Index -> assert false
                    (* トップレベルにindexの宣言を試みたらgen_using_fieldがエラーを発生させるはず *)
                | NVarEnv.Cell ->
                    let sel = Sel.base_or_mem diving nvar in
                    let nsel = Sel.to_nsel unknown_info sel in
                    Ir.Code.from_list [ Reset (nsel) ]
              ) |> List.flatten
            in
            (* dive中であればスコープの終わりでもセルを初期化する *)
            let code_clean_end =
              if diving <> None then code_clean else Ir.Code.from_list []
            in
            ((), code_clean @ code_child @ code_clean_end)
        | StLet (binding, st_list) ->
            let envs = Eval.eval_let_binding ~export:false ctx.envs binding in
            codegen { ctx with envs } st_list
        | StExpand ex_block ->
            let envs, st_list = Eval.eval envs ex_block |> to_block ex_block.i in
            codegen { ctx with envs } st_list
        | StDive (ex_ptr, st_list) ->
            let sel, _ = Eval.eval envs ex_ptr |> to_sel_and_nvar_env ex_ptr.i in
            let _ = Sel.to_nptr ex_ptr.i sel in
            (* ↑インデックスであることを確認している *)
            codegen { ctx with diving = Some sel } st_list
      ) () st_list
    in
    ((), List.flatten code_list)
  in
  let (), cmd_list = codegen ctx st_list in
  (nmain, cmd_list)

let gen_ir (dirname: string) (program: program) : Ir.Field.main * 'a Ir.Code.t =
  let toplevels, main = program in
  match main with
  | None -> error_at unknown_info "main not found"
  | Some main ->
      let envs = Eval.eval_toplevels dirname [] Eval.empty_envs toplevels in
      gen_ir_from_main envs main

let gen_bf_from_source path =
  let dirname = Filename.dirname path in
  let program = Eval.load_program path in
  let field, ir_code = gen_ir dirname program in
  (* 生存セル解析による最適化 *)
  let ir_code = Ir.Code.convert_idioms ir_code in
  let liveness = Ir.Liveness.analyze field ir_code in
  let graph = Ir.Liveness.Graph.create field liveness in
  let field, ir_code =
    Ir.Liveness.Graph.create_program_with_merged_cells
      graph field ir_code
  in
  (* メンバ並び順最適化 *)
  let mcounter = Ir.MovementCounter.from_code ir_code in
  (* bf生成 *)
  let layout = Ir.Layout.create mcounter field in
  Ir.BfGen.gen_bf layout ir_code