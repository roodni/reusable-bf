open Printf
open Lib
open Support.Error

let () =
  let flag_run = ref false in
  let flag_bf = ref false in
  let flag_show_layouts = ref false in
  let flag_dump_tape = ref false in
  let filename = ref "-" in
  Arg.parse
    [ ("-r", Set flag_run, " Run after compile");
      ("-b", Set flag_bf, " Load a brainfuck program instead of bf-reusable");
      ("--show-layouts", Set flag_show_layouts, " Show cell layouts");
      ("--dump-tape", Set flag_dump_tape, " Dump the brainfuck array after run");
    ]
    (fun s -> filename := s )
    (sprintf "Usage: %s <options> <file>" Sys.argv.(0));

  let bf_code =
    if !flag_bf then begin
      let channel = open_in !filename in
      let code = Bf.Code.parse (Stream.of_channel channel) in
      close_in channel;
      code
    end else begin
      let dirname = Filename.dirname !filename in
      let program = Reusable.load_program !filename in

      let field, code = Reusable.codegen_all dirname program in
      let layout = Named.Layout.from_field field in

      if !flag_show_layouts then begin
        print_endline "[";
        Format.printf "@[layout = ";
        Named.Layout.show Format.std_formatter layout;
        Format.print_flush ();
        print_endline "\n]";
      end;

      Named.gen_bf layout code
    end
  in

  if not !flag_run && not !flag_bf then begin
    print_endline @@ Bf.Code.to_string bf_code;
  end;

  if !flag_run || !flag_bf then begin
    let res, dump =
      Bf.Exe.run_stdio
        ~cell_type:WrapAround256
        (Bf.Exe.from_code bf_code)
    in
    if !flag_dump_tape then begin
      print_newline ();
      Bf.Exe.Dump.dump dump;
    end else begin
      match res with
      | Ok () -> ()
      | Error e ->
          eprintf "Execution error: %s" e;
          exit 1
    end
  end