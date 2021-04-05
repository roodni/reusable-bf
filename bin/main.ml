open Batteries
open Printf
open Lib

let main () =
  let filename = Sys.argv.(1) in
  let file_in = open_in filename in
  let program = Lexing.from_channel file_in |> Parser.program Lexer.main in
  close_in file_in;

  let dfn, cmd_list = Reusable.Program.codegen program in
  let layout = Named.Layout.of_dfn dfn in
  print_endline "[";
  print_endline "--- layout ---";
  Named.Layout.print layout;
  print_endline "]";

  let code = Named.codegen layout cmd_list in
  print_endline @@ Bf.Cmd.list_to_string code;
  print_endline "[-][";
  print_endline "--- start ---";
  let state = Bf.run_stdio code in
  print_newline ();
  Bf.State.dump state;
  print_endline "]"

let () = main ()