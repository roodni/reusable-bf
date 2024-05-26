open Support.Pervasive
open Support.Info
open Syntax
open Value

(* ソースコードを読み込むときは必ずこれを通す
   Parserのエラーハンドリングを行うが
   それはそうと、ファイルの形式がテキストじゃないとかの場合はimport側でエラーを出したいので
   区別できるように結果はResult型
*)
let load filename channel =
  let lexbuf = Lexer.create filename channel in
  match Parser.program Lexer.main lexbuf with
  | program ->
      SyntaxScan.scan_program ~pname:None program;
      Ok program
  | exception Parser.Error ->
      let info = !Lexer.curr_info |> Option.get in
      Error.top info Parser_Unexpected
  | exception Sys_error e -> Error e

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

let find_source path_limit curr_dir path =
  let exception R of Error.t in
  try
    ( match path_limit with
      | NoLimit -> ()
      | Limited l ->
          if not (List.mem path l) then raise @@ R Module_Limited_import
    );
    let paths =
      if not (Filename.is_relative path) then [path]
      else
        [ lib_path ]
        |> List.filter_map Fun.id
        |> List.cons curr_dir
        |> List.map (fun dir -> Filename.concat dir path)
    in
    let found_path = List.find_opt Sys.file_exists paths in
    match found_path with
    | Some p -> Ok p
    | None -> raise @@ R (Module_import_file_not_found path)
  with R e -> Error e

let load_from_source path =
  let channel = open_in path in
  Fun.protect (fun () -> load path channel)
    ~finally:(fun () -> close_in channel)


module FileMap = struct
  (* inodeとかをキーとするマップ
     読み込んだファイルの評価結果をキャッシュするほか
     循環参照の検出も行う
  *)
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
    curr_dirname: string;
    filemap: FileMap.t;
    path_limit: path_limit;
  }

let rec eval_decl ctx decl : ctx =
  (* print_endline ctx.curr_dirname;
  VE.to_seq ctx.ex_envs.va_env |> Seq.map fst |> Seq.map Var.to_string
  |> List.of_seq
  |> String.concat ", " |> print_endline; *)
  match decl.v with
  | DeclLet { binding; is_priv } ->
      let env = Eval.eval_let_binding ~recn:0 ctx.envs binding in
      { ctx with
        envs = Envs.extend_with_value_env env ctx.envs;
        ex_envs = if is_priv
          then ctx.ex_envs
          else Envs.extend_with_value_env env ctx.ex_envs;
      }
  | DeclLetRec { binding=(v, ex); is_priv } ->
      let env = Eval.eval_let_rec ~recn:0 ctx.envs v ex in
      { ctx with
        envs = Envs.extend_with_value_env env ctx.envs;
        ex_envs = if is_priv
          then ctx.ex_envs
          else Envs.extend_with_value_env env ctx.ex_envs;
      }
  | DeclOpen mod_ex ->
      let ctx, mod_envs = eval_mod_expr ctx mod_ex in
      { ctx with envs = Envs.import mod_envs ctx.envs }
  | DeclInclude mod_ex ->
      (* 封印中 *)
      let ctx, mod_envs = eval_mod_expr ctx mod_ex in
      { ctx with
        envs = Envs.import mod_envs ctx.envs;
        ex_envs = Envs.import mod_envs ctx.ex_envs;
      }
  | DeclModule { binding=(uv, mod_ex); is_priv } ->
      let ctx, mod_envs = eval_mod_expr ctx mod_ex in
      { ctx with
        envs = Envs.add_module_binding uv mod_envs ctx.envs;
        ex_envs = if is_priv
          then ctx.ex_envs
          else Envs.add_module_binding uv mod_envs ctx.ex_envs;
      }

and eval_decls ctx decls : ctx =
  List.fold_left eval_decl ctx decls

and eval_mod_expr ctx mod_expr =
  match mod_expr.v with
  | ModImport p -> begin
      let path =
        match find_source ctx.path_limit ctx.curr_dirname p with
        | Ok p -> p
        | Error e -> Error.top mod_expr.i e
      in
      match FileMap.find path ctx.filemap with
      | Some Loading -> Error.top mod_expr.i Module_Recursive_import
      | Some (Loaded envs) -> (ctx, envs)
      | None ->
          let ctx' = {
            envs = Envs.initial;
            ex_envs = Envs.empty;
            curr_dirname = Filename.dirname path;
            filemap = FileMap.add path Loading ctx.filemap;
            path_limit = NoLimit; (* import先でのimportは信用する *)
          } in
          let prog = match load_from_source path with
            | Ok p -> p
            | Error error -> Error.top mod_expr.i @@ Error.Module_import_failed_to_read { path; error }
          in
          let ctx' = eval_decls ctx' prog in
          let envs = ctx'.ex_envs in
          let filemap = FileMap.add path (Loaded envs) ctx'.filemap in
          ({ ctx with filemap }, envs)
    end
  | ModStruct prog ->
      let ctx' = {
        ctx with
        ex_envs = Envs.empty;
      } in
      let ctx' = eval_decls ctx' prog in
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
    curr_dirname = dirname;
    filemap = FileMap.empty;
    path_limit;
  } in
  let ctx = eval_decls ctx program in
  match VE.lookup (Var.of_string "main") ctx.ex_envs.va_env with
  | None -> Error.unknown Top_Missing_main
  | Some (VaBlock (envs, stmts)) -> IrGen.generate envs stmts
  | Some _ -> Error.unknown Top_main_is_not_stmts
;;

(** ファイルを読んでbfに変換する
    ハンドリングが雑なのでテスト用 *)
let gen_bf_from_source ?(path_limit=NoLimit) ?(opt_level=Ir.Opt.max_level) path =
  let dirname = Filename.dirname path in
  let program = load_from_source path |> Result.get_ok in
  let field, ir_code = gen_ir ~path_limit dirname program in
  let opt_context =
    Ir.Opt.{ field; code=ir_code; chan=stderr; dump=false }
  in
  let _, bf_code =
    Ir.Opt.codegen_by_level opt_level opt_context
  in
  bf_code