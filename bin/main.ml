open Printf
open Lib
open Support.Error

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
  let lexbuf = Lexer.create !filename file_in in
  let program =
      try Parser.program Lexer.main lexbuf with
      | Lexer.Error info -> error_at info "syntax error"
      | Parser.Error -> begin
          let p = Lexing.lexeme_start_p lexbuf in
          let info = info_of_position p in
          error_at info "unexpected token"
        end
  in
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
    end else begin
      match state.err with
      | None -> ()
      | Some e -> begin
          eprintf "execution error: %s" (Bf.Err.to_string e);
        end
    end;
    if state.err <> None then exit 1;
  end

let () = main ()