open Support.Pervasive
open Support.Info
open Syntax
open Value

open Printf

type ctx =
  { envs: envs;
    diving: Ir.Sel.index option;
      (* alloc文がフィールドを確保する位置 *)
    diving_fields: (Ir.Sel.index * irid_env) llist;
      (* インデックスとその下に確保されたフィールドの対応 *)
    recn: int;
  }


(* Fieldを読んでIr.Fieldを拡張しながらVarとIr.Idの対応を返す *)
let generate_field (nmain: Ir.Field.main) nfield field =
  let rec gen_using_field ?parent_name ~mergeable (nfield: Ir.Field.t) (field: Field.t): irid_env =
    field
    |> LList.fold_left
      (fun (env: irid_env) { i=info; v=(var, mtype) } : irid_env ->
        let nvar =
          Ir.Id.gen_named
            ( match parent_name with
              | None -> Var.to_string var
              | Some parent_name ->
                  sprintf "%s/%s" parent_name (Var.to_string var) )
        in
        match mtype with
        | Field.Cell ->
            Ir.Field.extend nfield nvar (Cell { ifable=false; mergeable });
            VE.extend var (withinfo info (nvar, MtyCell)) env
        | Field.Index ->
            if nfield == nmain.finite then
              Error.at info Gen_Alloc_Index_must_be_array_member;
            Ir.Field.extend nfield nvar Index;
            VE.extend var (withinfo info (nvar, MtyIndex)) env
        | Field.Array { length=Some length; mem } ->
            let nmembers = Ir.Field.empty () in
            let narray = Ir.Field.Array { length; members=nmembers } in
            Ir.Field.extend nfield nvar narray;
            let env_members = gen_using_field ~mergeable:false nmembers mem in
            VE.extend var (withinfo info (nvar, MtyArray { length=Some length; mem=env_members })) env
        | Field.Array { length=None; mem } ->
            if nfield != nmain.finite then
              Error.at info Gen_Alloc_Unlimited_array_cannot_be_array_member;
            let env_members = gen_using_field ~parent_name:(Var.to_string var) ~mergeable:false nmain.unlimited mem in
            VE.extend
              var
              (withinfo info
                (Ir.Field.uarray_id, MtyArray { length=None; mem=env_members }))
              env
      ) VE.empty
  in
  gen_using_field ~mergeable:true nfield field

let generate (envs : envs) (stmts: top_gen) : Ir.Field.main * unit Ir.Code.t =
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
            let nsel = eval envs ex_sel |> Va.to_cell ex_sel.i in
            let i =
              match ex_i_opt with
              | None -> 1
              | Some ex_i -> eval envs ex_i |>  Va.to_int ex_i.i
            in
            let code = Ir.Code.from_cmds [ Add (i * sign, nsel) ] in
            ((), code)
          end
        | StPut ex_sel ->
            let nsel = eval envs ex_sel |> Va.to_cell ex_sel.i in
            let code = Ir.Code.from_cmds [ Ir.Code.Put nsel ] in
            ((), code)
        | StGet ex_sel ->
            let nsel = eval envs ex_sel |> Va.to_cell ex_sel.i in
            let code = Ir.Code.from_cmds [ Get nsel ] in
            ((), code)
        | StWhile (sel_ex, stmts) -> begin
            let sel = eval envs sel_ex |> Va.to_cell sel_ex.i in
            let (), child_code = gen ctx stmts in
            let code =
              Ir.Code.from_cmds [ Loop (sel, child_code) ]
            in
            ((), code)
          end
        | StILoop (index_ex, stmts) ->
            let index = eval envs index_ex |> Va.to_index index_ex.i in
            let (), child_code = gen ctx stmts in
            let code =
              Ir.Code.from_cmds [ ILoop (index, child_code) ]
            in
            ((), code)
        | StIf (ex_sel, stmts_then, stmts_else) ->
            let nsel = eval envs ex_sel |> Va.to_cell ex_sel.i in
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
            let index = eval envs ex_idx |> Va.to_index ex_idx.i in
            let i = sign * match ex_i_opt with
              | None -> 1
              | Some ex_i ->
                  (* XXX: いまのところ2以上のシフトは未実装
                     Parserで無効にしている
                  *)
                  eval envs ex_i |> Va.to_int ex_i.i
            in
            (* シフト時に移動させられるdiving下の一時セルのid *)
            let followers =
              diving_fields
              |> LList.filter_map
                (fun (diving, irid_env) ->
                  if index = diving then (* セルの中身をコピーする *)
                    VE.to_seq irid_env
                    |> Seq.map
                      (fun (_, { v=(id, mtype); i=_ }) ->
                        match mtype with
                        | MtyCell -> id
                        | MtyArray _ | MtyIndex -> assert false
                            (* allocを試みた時点で弾かれるので到達できない *)
                      )
                    |> LList.of_seq |> Option.some
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
            let irid_env = generate_field nmain nmain.finite field in
            let va_env =
              Envs.extend_value_env_with_irid_env
                None irid_env envs.va_env
            in
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
            let irid_env = generate_field nmain irfield field in
            (* 確保するセルに対するゼロ初期化 *)
            let code_clean =
              VE.to_seq irid_env
              |> Seq.map
                (fun (_, { v=(id, mtype); i }) ->
                  match mtype with
                  | MtyCell ->
                      let sel = Ir.Sel.concat_member_to_index_opt_tail diving id 0 in
                      Ir.Code.from_cmds [ Reset sel ]
                  | MtyArray _ -> Error.at i Gen_Alloc_Array_not_implemented
                  | MtyIndex -> Error.at i Gen_Alloc_Index_must_be_array_member
                )
              |> LList.of_seq
              |> LList.concat
            in
            (* 後続のコード生成 *)
            let va_env =
              Envs.extend_value_env_with_irid_env
                diving irid_env envs.va_env
            in
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
            let envs, stmts = eval envs ex_block |> Va.to_block ex_block.i in
            gen { ctx with envs } stmts
        | StDive (index_ex, stmts) ->
            let index = eval envs index_ex |> Va.to_index index_ex.i in
            gen { ctx with diving = Some index } stmts
      ) () stmts
    in
    ((), code_llist |> LList.concat)
  in
  let (), code = gen ctx stmts in
  (nmain, code)