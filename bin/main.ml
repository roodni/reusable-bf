open Printf

(* コマンドライン引数 *)
let flag_bf = ref false
let flag_run = ref false
let flag_ir = ref false
let arg_optimize_level = ref Ir.Opt.max_level
let flag_print_opt = ref false
let channel_print_opt = ref stderr
let flag_show_layouts = ref false
let flag_dump_tape = ref false
let filename = ref ""

let parse_args () =
  let filenames = ref [] in
  let anon_fun s =
    filenames := s :: !filenames
  in
  let speclist = Arg.[
    ("-b", Set flag_bf, " Load and run a brainfuck program instead of bf-reusable programs");
    ("-r", Set flag_run, " Run a bf-reusable program after compilation");
    ( "--ir",
      Unit (fun () -> flag_ir := true; flag_run := true;),
      " Run a bf-reusable program using the IR interpreter");
    ("--dump-tape", Set flag_dump_tape, " Dump the brainfuck array after run");
    ("-v", Set flag_show_layouts, " Show detailed compilation information");
    ("--show-layout", Set flag_show_layouts, " ");
    ("--opt",
      Set_int arg_optimize_level,
      sprintf " Set optimization level (0-%d)" Ir.Opt.max_level);
    ("--print-opt", Set flag_print_opt, " ");
    (* ("--print-opt-o", String (fun s -> channel_print_opt := open_out s), " "); *)
    ("-", Unit (fun () -> anon_fun "-"), "");
  ] in
  (* --print-opt-oはファイルを上書きする危険性があるので消されているが、open_outのタイミングが悪いだけなので、そのうち復活させる *)
  let usage_msg =
    sprintf "Usage: %s <options> <file>" Sys.argv.(0)
  in
  Arg.parse speclist anon_fun usage_msg;

  (match !filenames with
    | [s] -> filename := s
    | _ -> Arg.usage speclist usage_msg; exit 2
  )
;;

let get_source () =
  if !filename = "-" then
    (Sys.getcwd (), stdin)
  else if not @@ Sys.file_exists !filename then begin
    Reusable.Error.print (`Info None, Module_import_file_not_found !filename);
    exit 1
  end else if Sys.is_directory !filename then begin
    Reusable.Error.print (`Info None, Module_import_file_is_directory !filename);
    exit 1
  end else
    (Filename.dirname !filename, open_in !filename)

(** bfのコードを実行する *)
let run_bf bf_code =
  let exe = Bf.Exe.from_code bf_code in
  let res, dump =
    Bf.Exe.run_stdio ~cell_type:WrapAround256 exe
  in
  if !flag_dump_tape then begin
    print_newline ();
    Bf.Exe.Dump.dump dump;
  end;
  begin
    match res with
    | Ok () -> ()
    | Error e ->
        eprintf "Execution error: %s\n" e;
        exit 1;
  end;
;;

let run_ir field ir_code =
  let res = Ir.Interpreter.run_stdio ~cell_type:WrapAround256 field ir_code in
  match res with
  | Ok () -> ()
  | Error e ->
      Ir.Interpreter.print_error e;
      exit 1
;;

(** bfのインタプリタとして使う場合の処理 *)
let use_as_bf_interpreter () =
  let bf_code =
    let _, channel = get_source () in
    let code =
      Seq.of_dispenser
        (fun () -> In_channel.input_char channel)
      |> Bf.Code.parse
    in
    close_in channel;
    code
  in
  run_bf bf_code
;;

(** bf-reusableのコンパイラとして使う場合の処理 *)
let use_as_bfr_compiler () =
  let dirname, channel = get_source () in
  let field, ir_code =
    try
      let program =
        Fun.protect (fun () -> Reusable.Program.load !filename channel)
          ~finally:(fun () -> close_in channel)
      in
      Reusable.Program.gen_ir dirname program
    with Reusable.Error.Exn_at e ->
      Reusable.Error.print e;
      exit 1
  in

  (* 最適化とコード生成 *)
  if !flag_ir && !flag_run then
    arg_optimize_level := 0;

  let opt_context =
    Ir.Opt.{field; code=ir_code; chan=(!channel_print_opt); dump=(!flag_print_opt)}
  in
  let layout, bf_code =
    Ir.Opt.codegen_by_level !arg_optimize_level opt_context
  in

  (* 詳細情報の出力 *)
  let output_bf_code_info chan =
    let ppf = Format.formatter_of_out_channel chan in
    Format.pp_open_vbox ppf 0;
    Format.fprintf ppf "[LAYOUT]@,";
    Ir.Layout.output ppf layout;
    Format.fprintf ppf "@,@,";
    Format.fprintf ppf "[CODE SIZE]@,";
    Format.fprintf ppf "%d bytes@," (Bf.Code.length bf_code);
    Format.pp_print_newline ppf ();
  in
  if !flag_print_opt then begin
    output_bf_code_info !channel_print_opt;
  end;
  if !flag_show_layouts then begin
    print_endline "[ === COMPILATION INFO ===";
    print_newline ();
    output_bf_code_info stdout;
    print_endline "]";
  end;

  (* コンパイル結果の出力または実行 *)
  if !flag_run then begin
    if !flag_ir
      then run_ir field ir_code
      else run_bf bf_code;
  end else begin
    Bf.Code.output_skelton print_char bf_code;
    flush stdout;
  end
;;

(* entrypoint *)
let () =
  parse_args ();
  if !flag_bf
    then use_as_bf_interpreter ()
    else use_as_bfr_compiler ()
;;