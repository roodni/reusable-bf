open Support.Error
open Syntax

let load filename =
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
      let toplevels, _ = load filename in
      eval_toplevels next_dirname (filename :: import_history) Eval.empty_envs toplevels
  in
  List.fold_left
    (fun envs toplevel ->
      match toplevel.v with
      | TopLet binding -> Eval.eval_let_binding ~export:true envs binding
      | TopImport filename ->
          let envs_imported = import_module toplevel.i filename in
          Eval.import_envs envs_imported envs
      | TopImportAs (filename, uv) ->
          let envs_imported = import_module toplevel.i filename |> Eval.export_envs in
          { envs with module_env = Eval.UVE.extend uv envs_imported envs.module_env }
    )
    envs toplevels

let gen_ir (dirname: string) (program: program) : Ir.Field.main * 'a Ir.Code.t =
  let toplevels, main = program in
  match main with
  | None -> error_at unknown_info "main not found"
  | Some main ->
      let envs = eval_toplevels dirname [] Eval.empty_envs toplevels in
      IrGen.generate envs main

let gen_bf_from_source path =
  let dirname = Filename.dirname path in
  let program = load path in
  let field, ir_code = gen_ir dirname program in
  (* 生存セル解析による最適化 *)
  let ir_code = Ir.Code.convert_idioms ir_code in
  let liveness = Ir.Liveness.analyze field ir_code in
  let graph = Ir.Liveness.Graph.create field liveness in
  let field, ir_code =
    Ir.Liveness.Graph.create_program_with_merged_cells
      graph field ir_code
  in
  (* 条件セルがゼロになるループの除去 *)
  let const_analysis_result = Ir.Const.analyze field ir_code in
  let ir_code = Ir.Const.eliminate_never_entered_loop const_analysis_result in
  (* メンバ並び順最適化 *)
  let mcounter = Ir.MovementCounter.from_code ir_code in
  (* bf生成 *)
  let layout = Ir.Layout.create mcounter field in
  Ir.BfGen.gen_bf layout ir_code