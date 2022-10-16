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

type ctx =
  { envs: Eval.envs;
    top_gen_opt: top_gen option;
    curr_dirname: string;
    path_history: string list;
  }
let empty_ctx curr_dirname =
  { envs = Eval.empty_envs;
    top_gen_opt = None;
    curr_dirname;
    path_history = [];
  }

let rec eval_toplevel ctx (toplevel: toplevel) : ctx =
  let import_module path =
    let path =
      if FilePath.is_relative path then
        FilePath.concat ctx.curr_dirname path
        |> FilePath.reduce ~no_symlink:true
      else path
    in
    let next_dirname = FilePath.dirname path in
    if List.mem path ctx.path_history then
      error_at toplevel.i "Recursive import"
    else
      let toplevels = load path in
      let ctx =
        eval_toplevels
          { envs = Eval.empty_envs;
            top_gen_opt = None;
            curr_dirname = next_dirname;
            path_history = path :: ctx.path_history;
          }
          toplevels
      in
      ctx.envs
  in
  match toplevel.v with
  | TopLet binding ->
      let envs = Eval.eval_let_binding ~export:true ctx.envs binding in
      { ctx with envs }
  | TopCodegen top_gen ->
      if ctx.top_gen_opt <> None then
        error_at toplevel.i "Duplicate codegen"
      else
        { ctx with top_gen_opt=Some top_gen }
  | TopImport filename ->
      let imported_envs = import_module filename in
      let envs = Eval.import_envs imported_envs ctx.envs in
      { ctx with envs }
  | TopImportAs (filename, uv) ->
      let imported_envs = import_module filename |> Eval.export_envs in
      let envs =
        { ctx.envs with
          module_env = Eval.UVE.extend uv imported_envs ctx.envs.module_env
        }
      in
      { ctx with envs }
and eval_toplevels ctx (toplevels: toplevel list) : ctx =
  List.fold_left eval_toplevel ctx toplevels

let gen_ir (dirname: string) (program: program) : Ir.Field.main * 'a Ir.Code.t =
  let ctx = empty_ctx dirname in
  let ctx = eval_toplevels ctx program in
  match ctx.top_gen_opt with
  | None -> error_at unknown_info "Missing codegen"
  | Some top_gen -> IrGen.generate ctx.envs top_gen

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