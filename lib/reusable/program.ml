open Support.Pervasive
open Support.Info
open Syntax
open Value

let load filename channel =
  let lexbuf = Lexer.create filename channel in
  let program =
    try Parser.program Lexer.main lexbuf with
    | Parser.Error -> begin
        let info = !Lexer.curr_info in
        Error.at info Parser_Unexpected
      end
  in
  validate_program_depth 0 program;
  program

let load_from_source path =
  let channel = open_in path in
  let res = load path channel in
  close_in channel;
  res


module FileMap = struct
  (* ファイルパス (の指すファイル) をキーとするマップ
     inodeとかを見る *)
  module M = Map.Make(struct
    type t = int * int
    let compare = compare
  end)

  type progress = Loading | Loaded of envs
  type t = progress M.t

  let stats_to_key (stats: Unix.stats) =
    (stats.st_ino, stats.st_dev)
  let empty : t = M.empty

  let add path v (m: t) =
    let key = stats_to_key (Unix.stat path) in
    M.update key
      (function
        | None | Some Loading -> Some v
        | Some (Loaded _) -> assert false
      ) m

  let find path (m: t) =
    let key = stats_to_key (Unix.stat path) in
    M.find_opt key m
end


type ctx =
  { envs: envs;
    ex_envs: envs; (* エクスポートされる環境 *)
    top_gen_opt: top_gen option;
    curr_dirname: string;
    filemap: FileMap.t;
    sandbox: bool;
  }
let init_ctx ~sandbox curr_dirname =
  { envs = Envs.empty;
    ex_envs = Envs.empty;
    top_gen_opt = None;
    curr_dirname;
    filemap = FileMap.empty;
    sandbox;
  }

let rec eval_toplevel ctx (toplevel: toplevel) : ctx =
  match toplevel.v with
  | TopLet binding ->
      let env = Eval.eval_let_binding ~recn:0 ctx.envs binding in
      { ctx with
        envs = Envs.extend_with_value_env env ctx.envs;
        ex_envs = Envs.extend_with_value_env env ctx.ex_envs;
      }
  | TopCodegen top_gen ->
      if Option.is_some ctx.top_gen_opt then
        Error.at toplevel.i Top_Duplicated_codegen
      else
        { ctx with top_gen_opt=Some top_gen }
  | TopOpen mod_ex ->
      let ctx, mod_envs = eval_mod_expr ctx mod_ex in
      { ctx with envs = Envs.import mod_envs ctx.envs }
  | TopInclude mod_ex ->
      (* 封印中 *)
      let ctx, mod_envs = eval_mod_expr ctx mod_ex in
      { ctx with
        envs = Envs.import mod_envs ctx.envs;
        ex_envs = Envs.import mod_envs ctx.ex_envs;
      }
  | TopModule (uv, mod_ex) ->
      let ctx, mod_envs = eval_mod_expr ctx mod_ex in
      { ctx with
        envs = Envs.add_module_binding uv mod_envs ctx.envs;
        ex_envs = Envs.add_module_binding uv mod_envs ctx.ex_envs;
      }
and eval_toplevels ctx (toplevels: toplevel llist) : ctx =
  LList.fold_left eval_toplevel ctx toplevels
and eval_mod_expr ctx mod_expr =
  match mod_expr.v with
  | ModImport p -> begin
      if ctx.sandbox then Error.at mod_expr.i Module_Sandbox_import;
      let path =
        if Filename.is_relative p then
          Filename.concat ctx.curr_dirname p
        else p
      in
      if not (Sys.file_exists path) then
        Error.at mod_expr.i (Module_import_file_not_found p);
      match FileMap.find path ctx.filemap with
      | Some Loading -> Error.at mod_expr.i Module_Recursive_import
      | Some (Loaded envs) -> (ctx, envs)
      | None ->
          let ctx' = {
            envs = Envs.empty;
            ex_envs = Envs.empty;
            top_gen_opt = None;
            curr_dirname = Filename.dirname path;
            filemap = FileMap.add path Loading ctx.filemap;
            sandbox = ctx.sandbox;
          } in
          let prog = load_from_source path in
          let ctx' = eval_toplevels ctx' prog in
          let envs = ctx'.ex_envs in
          let filemap = FileMap.add path (Loaded envs) ctx'.filemap in
          ({ ctx with filemap }, envs)
    end
  | ModStruct prog ->
      let ctx' = {
        ctx with
        ex_envs = Envs.empty;
        top_gen_opt = None;
      } in
      let ctx' = eval_toplevels ctx' prog in
      ({ ctx with filemap = ctx'.filemap }, ctx'.ex_envs)
  | ModVar l -> begin
      assert (l <> lnil);
      let envs =
        LList.fold_left
          (fun envs u ->
            match UVE.lookup u envs.module_env with
            | None -> Error.at mod_expr.i (Eval_Module_not_defined u)
            | Some envs -> envs
          ) ctx.envs l
      in
      (ctx, envs)
    end

let gen_ir ~sandbox (dirname: string) (program: program)
    : Ir.Field.main * 'a Ir.Code.t =
  let ctx = init_ctx ~sandbox dirname in
  let ctx = eval_toplevels ctx program in
  match ctx.top_gen_opt with
  | None -> Error.at unknown_info Top_Missing_codegen
  | Some top_gen -> IrGen.generate ctx.envs top_gen

let gen_bf_from_source ?(sandbox=false) path =
  let dirname = Filename.dirname path in
  let program = load_from_source path in
  let field, ir_code = gen_ir dirname ~sandbox program in
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