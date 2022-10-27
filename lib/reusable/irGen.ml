open Support.Pervasive
open Support.Info
open Syntax

type ctx =
  { envs: Eval.envs;
    diving: Ir.Sel.index option;
      (* alloc文がフィールドを確保する位置 *)
    diving_fields: (Ir.Sel.index * IrIdEnv.t) llist;
      (* インデックスとその下に確保されたフィールドの対応 *)
    recn: int;
  }

let generate (envs : Eval.envs) (stmts: top_gen) : Ir.Field.main * unit Ir.Code.t =
  let open Eval.Value in
  let nmain = Ir.Field.empty_main () in
  let ctx = { envs; diving=None; diving_fields=lnil; recn=0 } in
  let rec gen (ctx: ctx) (stmts: stmts): unit * unit Ir.Code.t =
    let { envs; diving; diving_fields; _ } = ctx in
    let (), code_llist =
      LList.fold_left_map (fun () stmt : (unit * unit Ir.Code.t) ->
        let info = stmt.i in
        let gen ctx stmts =
          let ctx = { ctx with recn = ctx.recn + 1 } in
          if ctx.recn > 10000 then
            Error.at info Memory_Recursion_limit;
          gen ctx stmts
        in
        let eval = Eval.eval ~recn:0 in
        match stmt.v with
        | StAdd (sign, ex_sel, ex_i_opt) -> begin
            (* nsel というのは Ir.Sel の旧名の Named.Sel のこと *)
            let nsel = eval envs ex_sel |> to_cell ex_sel.i in
            let i =
              match ex_i_opt with
              | None -> 1
              | Some ex_i -> eval envs ex_i |>  to_int ex_i.i
            in
            let code = Ir.Code.from_cmds [ Add (i * sign, nsel) ] in
            ((), code)
          end
        | StPut ex_sel ->
            let nsel = eval envs ex_sel |> to_cell ex_sel.i in
            let code = Ir.Code.from_cmds [ Ir.Code.Put nsel ] in
            ((), code)
        | StGet ex_sel ->
            let nsel = eval envs ex_sel |> to_cell ex_sel.i in
            let code = Ir.Code.from_cmds [ Get nsel ] in
            ((), code)
        | StWhile (sel_ex, stmts) -> begin
            let sel = eval envs sel_ex |> to_cell sel_ex.i in
            let (), child_code = gen ctx stmts in
            let code =
              Ir.Code.from_cmds [ Loop (sel, child_code) ]
            in
            ((), code)
          end
        | StILoop (index_ex, stmts) ->
            let index = eval envs index_ex |> to_index index_ex.i in
            let (), child_code = gen ctx stmts in
            let code =
              Ir.Code.from_cmds [ ILoop (index, child_code) ]
            in
            ((), code)
        | StIf (ex_sel, stmts_then, stmts_else) ->
            let nsel = eval envs ex_sel |> to_cell ex_sel.i in
            let nmtype = Ir.Sel.find_mtype nmain nsel in
            (match nmtype with
            | Ir.Field.Cell cell ->  cell.ifable <- true
            | _ -> assert false
                (* to_cellの時点で弾かれて到達しないはず *)
            );
            let (), code_then = gen ctx stmts_then in
            let (), code_else = match stmts_else with
              | None -> ((), Ir.Code.from_cmds [])
              | Some stmts_else -> gen ctx stmts_else
            in
            let code =
              Ir.Code.from_cmds [ If (nsel, code_then, code_else) ] in
            ((), code)
        | StShift (sign, ex_idx, ex_i_opt) ->
            let index = eval envs ex_idx |> to_index ex_idx.i in
            let i = sign * match ex_i_opt with
              | None -> 1
              | Some ex_i ->
                  (* XXX: いまのところ2以上のシフトは未実装
                     Parserで無効にしている
                  *)
                  eval envs ex_i |> to_int ex_i.i
            in
            (* シフト時に移動させられるdiving下の一時セルのid *)
            let followers =
              diving_fields
              |> LList.filter_map
                (fun (diving, irid_env) ->
                  if index = diving then (* セルの中身をコピーする *)
                    IrIdEnv.to_llist irid_env |>
                    LList.map
                      (fun (_, { v=(id, mtype); i=_ }) ->
                        match mtype with
                        | IrIdEnv.Cell -> id
                        | IrIdEnv.Array _ | Index -> assert false
                            (* allocを試みた時点で弾かれるので到達できない *)
                      )
                    |> Option.some
                  else if Ir.Sel.is_via_index index (fst diving) then
                    Error.at info Gen_Shift_interfere
                  else None
                )
              |> LList.concat
            in
            let code_shift =
              Ir.Code.from_cmds [ Shift { n=i; index; followers } ]
            in
            ((), code_shift)
        | StBuild (field, stmts) ->
            let irid_env = IrIdEnv.gen_using_field nmain nmain.finite field in
            let va_env = extend_env_with_irid_env None irid_env envs.va_env in
            let ctx = { ctx with envs={ envs with va_env } } in
            let (), child_code = gen ctx stmts in
            ((), child_code)
        | StAlloc (field, stmts) ->
            let irfield = match diving with
              | None -> nmain.finite
              | Some (arr_sel, _) -> begin
                  match Ir.Sel.find_mtype nmain arr_sel with
                  | Ir.Field.Array { members; _ } -> members
                  | Cell _ | Index -> assert false
                      (* $diveでインデックス以外がdivingに指定されることは弾かれる *)
                end
            in
            let irid_env = IrIdEnv.gen_using_field nmain irfield field in
            (* 確保するセルに対するゼロ初期化 *)
            let code_clean =
              IrIdEnv.to_llist irid_env
              |> LList.map
                (fun (_, { v=(id, mtype); i }) ->
                  match mtype with
                  | IrIdEnv.Cell ->
                      let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
                      Ir.Code.from_cmds [ Reset sel ]
                  | Array _ -> Error.at i Gen_Alloc_Array_not_implemented
                  | Index -> Error.at i Gen_Alloc_Index_must_be_array_member
                )
              |> LList.concat
            in
            (* 後続のコード生成 *)
            let va_env = extend_env_with_irid_env diving irid_env envs.va_env in
            let envs = { envs with va_env } in
            let diving_fields = match diving with
              | None -> diving_fields
              | Some sel -> lcons (sel, irid_env) diving_fields
            in
            let ctx = { ctx with envs; diving_fields; } in
            let (), code_child = gen ctx stmts in
            (* ゼロ初期化は開始時と終了時に行う
               IRの最適化である程度消える *)
            ((), code_clean @+ code_child @+ code_clean)
        | StExpand ex_block ->
            let envs, stmts = eval envs ex_block |> to_block ex_block.i in
            gen { ctx with envs } stmts
        | StDive (index_ex, stmts) ->
            let index = eval envs index_ex |> to_index index_ex.i in
            gen { ctx with diving = Some index } stmts
      ) () stmts
    in
    ((), code_llist |> LList.concat)
  in
  let (), code = gen ctx stmts in
  (nmain, code)