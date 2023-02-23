open Printf
open Support.Info

(* コマンドライン引数 *)
let flag_bf = ref false
let flag_run = ref false
let flag_ir = ref false
let arg_optimize_level = ref 10
let flag_verbose = ref false
let flag_show_liveness = ref false
let flag_show_layouts = ref false
let flag_show_possible_cell_values = ref false
let flags_compile_information =
  [flag_show_liveness; flag_show_layouts; flag_show_possible_cell_values]
let flag_dump_tape = ref false
let flag_sandbox = ref false
let arg_limit_import_paths = ref None
let flag_stdin = ref false
let filename = ref ""
let parse_args () =
  let speclist = Arg.[
    ("-b", Set flag_bf, " Load and run the brainfuck program instead of bf-reusable programs");
    ("-r", Set flag_run, " Run the bf-reusable program after compilation");
    ("--ir", Set flag_ir, " Use IR interpreter");
    ("-v", Set flag_verbose, " Show detailed compilation information");
    ("--show-liveness", Set flag_show_liveness, " Show the result of liveness analysis");
    ("--show-layout", Set flag_show_layouts, " Show cell layouts");
    ("--show-cell-values", Set flag_show_possible_cell_values, " ");
    ("--optimize", Set_int arg_optimize_level, " Set the optimization level (0-3)");
    ("--dump-tape", Set flag_dump_tape, " Dump the brainfuck array after run");
    ("--sandbox", Set flag_sandbox, " ");
    ("--limit-import-paths",
      String (fun s ->
        let l = String.split_on_char ',' s in
        arg_limit_import_paths := Some l ),
      " "
    );
    ("--stdin", Set flag_stdin, " ");
  ] in
  let usage_msg =
    sprintf "Usage: %s <options> <file>" Sys.argv.(0)
  in
  Arg.parse speclist (fun s -> filename := s ) usage_msg;

  if !flag_verbose then begin
    List.iter (fun r -> r := true) flags_compile_information;
  end;

  if Array.length Sys.argv = 1 then begin
    Arg.usage speclist usage_msg;
    exit 2
  end;
;;

let get_source () =
  if !flag_stdin then
    (Sys.getcwd (), stdin)
  else
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
  | Error (info, msg) ->
      Format.eprintf "@[<v>";
      output_info Format.err_formatter info;
      Format.eprintf "  Execution error: %s@." msg;
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
  let path_limit =
    match !arg_limit_import_paths with
    | None ->
        if !flag_sandbox
          then Reusable.Program.Limited []
          else NoLimit
    | Some l -> Limited l
  in
  let field, ir_code =
    try
      let program = Reusable.Program.load !filename channel in
      close_in channel;
      Reusable.Program.gen_ir ~path_limit dirname program
    with Reusable.Error.Exn_at msg_wi ->
      Reusable.Error.print msg_wi;
      exit 1
  in

  (* TODO: IR最適化をmainに書くのをやめる *)
  if !flag_ir && !flag_run then
    arg_optimize_level := 0;

  (* 生存セル解析による最適化 *)
  let field, ir_code, liveness_opt =
    if !arg_optimize_level < 2 then (field, ir_code, None)
    else
      let ir_code = Ir.Code.convert_idioms ir_code in
      let liveness = Ir.Liveness.analyze field ir_code in
      let graph = Ir.Liveness.Graph.create field liveness in
      let field, ir_code =
        Ir.Liveness.Graph.create_program_with_merged_cells graph field ir_code
      in
      (field, ir_code, Some (liveness, graph))
  in

  (* 条件セルがゼロになるループの除去 *)
  let ir_code, const_analysis_opt =
    if !arg_optimize_level < 3 then (ir_code, None)
    else
      let result = Ir.Const.analyze field ir_code in
      let ir_code = Ir.Const.eliminate_never_entered_loop result in
      (Ir.Code.delete_annot ir_code, Some result)
  in

  (* セル並び順最適化 *)
  let mcounter =
    if !arg_optimize_level < 1
      then Ir.MovementCounter.empty ()
      else Ir.MovementCounter.from_code ir_code
  in

  (* bf生成 *)
  let layout = Ir.Layout.create mcounter field in
  let bf_code = Ir.BfGen.gen_bf layout ir_code in
  if !flag_sandbox && Bf.Code.length bf_code > 100000 then begin
    eprintf "The output code size is too large\n";
    exit 1;
  end;
  let bf_code_buf = Bf.Code.to_buffer bf_code in

  (* 詳細情報の出力 *)
  if List.exists (!) flags_compile_information then begin
    print_endline "[ --- COMPILATION INFO ---";
    print_newline ();

    if !flag_show_liveness then begin
      match liveness_opt with
      | None -> ()
      | Some (liveness, graph) ->
          print_endline "[LIVENESS]";
          Ir.Liveness.output_analysis_result Format.std_formatter liveness;
          Format.print_flush ();
          print_endline "\n";

          Format.printf "@[<hov>";
          Ir.Liveness.Graph.output_dot Format.std_formatter graph;
          Format.printf "@]";
          Format.print_flush ();
          print_endline "\n";
    end;

    if !flag_show_possible_cell_values then begin
      match const_analysis_opt with
      | None -> ()
      | Some result ->
          print_endline "[POSSIBLE CELL VALUES]";
          Format.printf "@[<v>";
          Ir.Const.output_analysis_result Format.std_formatter result;
          Format.printf "@]";
          Format.print_flush ();
          print_endline "\n";
    end;

    (* let tbl = Ir.MovementCounter.from_code ir_code in
    Ir.MovementCounter.dump tbl;
    print_newline (); *)

    if !flag_show_layouts then begin
      Format.printf "@[<v>[LAYOUT]@,";
      Ir.Layout.output Format.std_formatter layout;
      Format.printf "@]";
      Format.print_flush ();
      print_endline "\n";
    end;

    print_endline "[CODE SIZE]";
    printf "%d bytes\n" (Buffer.length bf_code_buf);
    print_newline ();

    print_endline "]";
  end;

  (* コンパイル結果の出力または実行 *)
  if !flag_run then begin
    if !flag_ir
      then run_ir field ir_code
      else run_bf bf_code;
  end else begin
    Buffer.output_buffer stdout bf_code_buf;
    print_newline ();
  end
;;

(* entrypoint *)
let () =
  parse_args ();
  if !flag_bf then use_as_bf_interpreter ()
  else begin
    if not !flag_sandbox then use_as_bfr_compiler ()
    else begin
      (* ヒープ使用量に制限をかけて実行する *)
      let handler () =
        let heapsize =
          (Gc.quick_stat ()).heap_words * Sys.word_size / 8
        in
        if heapsize > 20_000_000 then
          raise Out_of_memory;
      in
      let alarm = Gc.create_alarm handler in
      try
        Fun.protect use_as_bfr_compiler
          ~finally:(fun () -> Gc.delete_alarm alarm)
      with Out_of_memory ->
        eprintf "Heap usage exceeded the limit\n";
        exit 1;
    end;
  end;
;;