module Testcase = Testcase

(** ソースコードを読んで一気にbfに変換する *)
let gen_bf_from_source ?(opt_level=Ir.Opt.max_level) path =
  let lib_dirs = Cli.default_lib_dirs Sys.getenv_opt in
  let base_dir = Filename.dirname path in
  let program = Cli.Program.load_by_path path in
  let field, ir_code = Cli.Program.gen_ir ~lib_dirs ~base_dir program in
  let opt_context =
    Ir.Opt.{ field; code=ir_code; chan=stderr; dump=false }
  in
  let _, bf_code =
    Ir.Opt.codegen_by_level opt_level opt_context
  in
  bf_code
