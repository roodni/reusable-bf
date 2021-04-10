open Batteries
open Printf
open Lib

let main () =
  let flag_run = ref false in
  let flag_verbose = ref false in
  let filename = ref "-" in
  Arg.parse [
    ("-r", Set flag_run, "run");
    ("-v", Set flag_verbose, "verbose");
  ] (fun s -> filename := s ) "usage";

  let file_in =
    if !filename = "-" then stdin
    else open_in !filename
  in
  let program = Lexing.from_channel file_in |> Parser.program Lexer.main in
  close_in file_in;

  let dfn, cmd_list = Reusable.Codegen.codegen program in
  let layout = Named.Layout.of_dfn dfn in

  if !flag_verbose then begin
    print_endline "[";
    print_endline "--- layout ---";
    Named.Layout.print layout;
    print_endline "]";
  end;

  let code = Named.codegen layout cmd_list in

  if !flag_verbose || not !flag_run then begin
    print_endline @@ Bf.Cmd.list_to_string code;
  end;

  if !flag_run then begin
    let state = Bf.run_stdio code in
    if !flag_verbose then begin
      print_newline ();
      Bf.State.dump state;
    end;
  end

let () = main ()