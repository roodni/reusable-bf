open Support.Pervasive
open Support.Info
open Syntax
open Value

(* open Printf *)

type ctx =
  { envs: envs;
    diving: Ir.Sel.index option;
      (* alloc文がフィールドを確保する位置 *)
    diving_fields: (Ir.Sel.index * irid_env) list;
      (* インデックスとその下に確保されたフィールドの対応 *)
    recn: int;
    trace: trace;
  }


(* Fieldを読んでIr.Fieldを拡張しながらVarとIr.Idの対応を返す *)
let generate_field ~alloc ctx (ir_main: Ir.Field.main) field =
  let ir_field =
    if alloc then
      match ctx.diving with
      | None -> ir_main.finite
      | Some (arr_sel, _) -> begin
          match Ir.Field.find_by_sel ir_main arr_sel with
          | Ir.Field.Array { members; _ } -> members
          | Cell _ | Index -> assert false
              (* $diveでインデックス以外がdivingに指定されることは弾かれる *)
        end
    else ir_main.finite
  in
  let diving_index = if alloc then ctx.diving else None in
  let rec gen ~outermost ~sticky (nfield: Ir.Field.t) (field: field): irid_env =
    (* outermost: 構文的に最も外側のメンバかどうか *)
    field
    |> List.fold_left
      (fun (env: irid_env) { i=info; v=(var, mtype) } : irid_env ->
        let trace = push_info info ctx.trace in
        let pname = match get_pname_of_info info with
          | _ when not outermost -> None
          | Some "codegen" -> None
          | p -> p
        in
        let irvar = Ir.Id.gen_named (Var.to_string var) pname in
        match mtype with
        | MtyExCell ->
            Ir.Field.extend nfield irvar
              (Cell { ifable=false; sticky; index=diving_index });
            VE.extend var (irvar, MtyCell) env
        | MtyExIndex ->
            if nfield == ir_main.finite || alloc then
              Error.at trace Gen_Alloc_Index_must_be_array_member;
            Ir.Field.extend nfield irvar Index;
            VE.extend var (irvar, MtyIndex) env
        | MtyExArray _ when alloc ->
            Error.at trace Gen_Alloc_Array_not_implemented
        | MtyExArray { length=Some length_ex; mem } ->
            let length = Eval.eval ~recn:0 ctx.envs length_ex |> Va.to_int ctx.trace length_ex.i in
            if length < 0 then
              Error.at trace Gen_Field_Array_length_cannot_be_negative;
            let nmembers = Ir.Field.empty () in
            let narray = Ir.Field.Array { length; members=nmembers } in
            Ir.Field.extend nfield irvar narray;
            let env_members = gen ~outermost:false ~sticky:false nmembers mem in
            VE.extend var (irvar, MtyArray { length=Some length; mem=env_members }) env
        | MtyExArray { length=None; mem } ->
            if nfield != ir_main.finite then
              Error.at trace Gen_Field_Unlimited_array_cannot_be_array_member;
            let env_members = gen ~outermost:false ~sticky:false ir_main.unlimited mem in
            VE.extend
              var
              (Ir.Field.uarray_id, MtyArray { length=None; mem=env_members })
              env
      ) VE.empty
  in
  gen ~outermost:true ~sticky:true ir_field field

let generate (envs : envs) (stmts: stmts) : Ir.Field.main * unit Ir.Code.t =
  let nmain = Ir.Field.empty_main () in
  let ctx = { envs; diving=None; diving_fields=[]; recn=0; trace=empty_trace } in
  let rec gen (ctx: ctx) (stmts: stmts): unit * unit Ir.Code.t =
    let { envs; trace; _ } = ctx in
    let (), code_list =
      List.fold_left_map (fun () stmt : (unit * unit Ir.Code.t) ->
        let trace' = push_info stmt.i trace in
        let gen ctx stmts =
          let ctx = { ctx with recn = ctx.recn + 1 } in
          if ctx.recn > 10000 then
            Error.at trace' Memory_Recursion_limit;
          gen ctx stmts
        in
        let eval = Eval.eval ~recn:0 in
        match stmt.v with
        | StAdd (sign, ex_sel, ex_i_opt) -> begin
            (* nsel というのは Ir.Sel の旧名の Named.Sel のこと *)
            let nsel = eval envs ex_sel |> Va.to_cell trace ex_sel.i in
            let i =
              match ex_i_opt with
              | None -> 1
              | Some ex_i -> eval envs ex_i |>  Va.to_int trace ex_i.i
            in
            let code = Ir.Code.from_cmds trace' [ Add (i * sign, nsel) ] in
            ((), code)
          end
        | StPut ex_sel ->
            let nsel = eval envs ex_sel |> Va.to_cell trace ex_sel.i in
            let code = Ir.Code.from_cmds trace' [ Ir.Code.Put nsel ] in
            ((), code)
        | StGet ex_sel ->
            let nsel = eval envs ex_sel |> Va.to_cell trace ex_sel.i in
            let code = Ir.Code.from_cmds trace' [ Get nsel ] in
            ((), code)
        | StWhile (sel_ex, stmts) -> begin
            let sel = eval envs sel_ex |> Va.to_cell trace sel_ex.i in
            let (), child_code = gen ctx stmts in
            let code =
              Ir.Code.from_cmds trace' [ Loop (sel, child_code) ]
            in
            ((), code)
          end
        | StIndexLoop (index_ex, stmts) ->
            let index = eval envs index_ex |> Va.to_index trace index_ex.i in
            let (), child_code = gen ctx stmts in
            let code =
              Ir.Code.from_cmds trace' [ IndexLoop (index, child_code) ]
            in
            ((), code)
        | StIndexIf (index_ex, stmts) ->
            let index = eval envs index_ex |> Va.to_index trace index_ex.i in
            let (), child_code = gen ctx stmts in
            let code =
              Ir.Code.from_cmds trace' [ IndexIf (index, child_code) ]
            in
            ((), code)
        | StIf (ex_sel, stmts_then, stmts_else) ->
            let nsel = eval envs ex_sel |> Va.to_cell trace ex_sel.i in
            let nmtype = Ir.Field.find_by_sel nmain nsel in
            (match nmtype with
            | Ir.Field.Cell cell ->  cell.ifable <- true
            | _ -> assert false
                (* to_cellの時点で弾かれて到達しないはず *)
            );
            let (), code_then = gen ctx stmts_then in
            let (), code_else = match stmts_else with
              | None -> ((), Ir.Code.from_cmds trace' [])
              | Some stmts_else -> gen ctx stmts_else
            in
            let code =
              Ir.Code.from_cmds trace' [ If (nsel, code_then, code_else) ] in
            ((), code)
        | StShift (sign, ex_idx, ex_i_opt) ->
            let index = eval envs ex_idx |> Va.to_index trace ex_idx.i in
            let i = sign * match ex_i_opt with
              | None -> 1
              | Some ex_i ->
                  eval envs ex_i |> Va.to_int trace ex_i.i
            in
            (* シフト時に移動させられるdiving下の一時セルのid *)
            let followers =
              ctx.diving_fields
              |> List.filter_map
                (fun (diving, irid_env) ->
                  if index = diving then (* セルの中身をコピーする *)
                    VE.to_seq irid_env
                    |> Seq.map
                      (fun (_, (id, mtype)) ->
                        match mtype with
                        | MtyCell -> id
                        | MtyArray _ | MtyIndex -> assert false
                            (* allocを試みた時点で弾かれるので到達できない *)
                      )
                    |> List.of_seq |> Option.some
                  else if Ir.Sel.is_via_index index (fst diving) then
                    Error.at (push_info stmt.i trace) Gen_Shift_interfere
                  else None
                )
              |> List.concat
            in
            let code_shift =
              Ir.Code.from_cmds trace' [ Shift { n=i; index; followers } ]
            in
            ((), code_shift)
        | StBuild (field, stmts) ->
            let irid_env =
              generate_field ~alloc:false ctx nmain field
            in
            let va_env =
              Envs.extend_value_env_with_irid_env
                None irid_env envs.va_env
            in
            let ctx = { ctx with envs={ envs with va_env } } in
            let (), child_code = gen ctx stmts in
            ((), child_code)
        | StAlloc (field, stmts) ->
            let irid_env =
              generate_field ~alloc:true ctx nmain field
            in
            (* 確保するセルの初期化・後処理 *)
            let gen_for_allocated_cells f : unit Ir.Code.t =
              VE.to_seq irid_env
              |> Seq.map
                (fun (_, (id, mtype)) ->
                  match mtype with
                  | MtyCell ->
                      let sel = Ir.Sel.concat_member_to_index_opt_tail ctx.diving id 0 in
                      Ir.Code.from_cmds trace' (f sel)
                  | MtyArray _ | MtyIndex ->
                      (* field評価時にallocフラグが真であれば弾かれる *)
                      assert false
                )
              |> List.of_seq
              |> List.concat
            in
            let code_init = gen_for_allocated_cells (fun sel -> [Reset sel]) in
            let code_end = gen_for_allocated_cells (fun sel -> [Reset sel; Use sel]) in
            (* 子コードの生成 *)
            let va_env =
              Envs.extend_value_env_with_irid_env
                ctx.diving irid_env envs.va_env
            in
            let envs = { envs with va_env } in
            let diving_fields = match ctx.diving with
              | None -> ctx.diving_fields
              | Some sel -> (sel, irid_env) :: ctx.diving_fields
            in
            let ctx = { ctx with envs; diving_fields; } in
            let (), code_child = gen ctx stmts in
            (* ゼロ初期化は開始時と終了時に行う。
               開始時の初期化は最適化のため。
               終了時の初期化は何のためにやってるのか忘れた。なんだっけ？
               IRの最適化である程度消える *)
            ((), code_init @ code_child @ code_end)
        | StExpand { ex_stmts; req_trace } ->
            let envs, stmts = eval envs ex_stmts |> Va.to_block trace ex_stmts.i in
            let trace =
              if req_trace then push_info stmt.i trace
              else trace
            in
            gen { ctx with envs; trace; } stmts
        | StUnit expr ->
            let () = eval envs expr |> Va.to_unit trace expr.i in
            ((), Ir.Code.from_cmds trace' [])
        | StDive (index_ex_opt, stmts) ->
            let index =
              index_ex_opt
              |> Option.map
                (fun index_ex ->
                  eval envs index_ex
                  |> Va.to_index trace index_ex.i )
            in
            gen { ctx with diving = index } stmts
      ) () stmts
    in
    ((), code_list |> List.concat)
  in
  let (), code = gen ctx stmts in
  (nmain, code)