open Support.Pervasive
open Support.Info
open Syntax
open Value

let load filename channel =
  let lexbuf = Lexer.create filename channel in
  let program =
    try Parser.program Lexer.main lexbuf with
    | Parser.Error -> begin
        match !Lexer.curr_info with
        | Some info ->  Error.top info Parser_Unexpected
        | None -> assert false
      end
  in
  scan_program ~pname:None 0 program;
  program

let lib_path =
  [ Sys.getenv_opt "BFRE_LIB_PATH";
    Sys.getenv_opt "DUNE_SOURCEROOT"
      |> Option.map (fun r -> Filename.concat r "examples/lib");
    Sys.getenv_opt "OPAM_SWITCH_PREFIX"
      |> Option.map (fun r -> Filename.concat r "share/bf-reusable/lib");
  ]
  |> List.find_map Fun.id

type path_limit =
  | NoLimit
  | Limited of string list

let find_source info path_limit curr_dir path =
  ( match path_limit with
    | NoLimit -> ()
    | Limited l ->
        if not (List.mem path l) then
          Error.top info (Module_Limited_import)
  );
  if not (Filename.is_relative path) then path
  else begin
    let lib_dirs =
      [ Some curr_dir; lib_path ] |> List.filter_map Fun.id
    in
    let found_path =
      List.find_map
        (fun dir ->
          let path = Filename.concat dir path in
          if Sys.file_exists path then Some path else None
        )
        lib_dirs
    in
    match found_path with
    | Some p -> p
    | None -> Error.top info (Module_import_file_not_found path)
  end

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
    path_limit: path_limit;
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
        Error.top toplevel.i Top_Duplicated_codegen
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
and eval_toplevels ctx (toplevels: toplevel list) : ctx =
  List.fold_left eval_toplevel ctx toplevels
and eval_mod_expr ctx mod_expr =
  match mod_expr.v with
  | ModImport p -> begin
      let path =
        find_source mod_expr.i ctx.path_limit ctx.curr_dirname p
      in
      match FileMap.find path ctx.filemap with
      | Some Loading -> Error.top mod_expr.i Module_Recursive_import
      | Some (Loaded envs) -> (ctx, envs)
      | None ->
          let ctx' = {
            envs = Envs.initial;
            ex_envs = Envs.empty;
            top_gen_opt = None;
            curr_dirname = Filename.dirname path;
            filemap = FileMap.add path Loading ctx.filemap;
            path_limit = NoLimit; (* import先でのimportは信用する *)
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
      assert (l <> []);
      let envs =
        List.fold_left
          (fun envs u ->
            match UVE.lookup u envs.module_env with
            | None -> Error.top mod_expr.i (Eval_Module_not_defined u)
            | Some envs -> envs
          ) ctx.envs l
      in
      (ctx, envs)
    end

let gen_ir ~path_limit (dirname: string) (program: program)
    : Ir.Field.main * 'a Ir.Code.t =
  let ctx = {
    envs = Envs.initial;
    ex_envs = Envs.empty;
    top_gen_opt = None;
    curr_dirname = dirname;
    filemap = FileMap.empty;
    path_limit;
  } in
  let ctx = eval_toplevels ctx program in
  match ctx.top_gen_opt with
  | None -> Error.unknown Top_Missing_codegen
  | Some top_gen -> IrGen.generate ctx.envs top_gen

let gen_bf_from_source ?(path_limit=NoLimit) ?(opt_level=Ir.Opt.max_level) path =
  let dirname = Filename.dirname path in
  let program = load_from_source path in
  let field, ir_code = gen_ir ~path_limit dirname program in
  let opt_context =
    Ir.Opt.{ field; code=ir_code; chan=stderr; dump=false }
  in
  let _, bf_code =
    Ir.Opt.codegen_by_level opt_level opt_context
  in
  bf_code