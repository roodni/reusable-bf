open Printf
open Lib
open Support.Error

let () =
  let flag_run = ref false in
  let flag_verbose = ref false in
  let filename = ref "-" in
  Arg.parse [
    ("-r", Set flag_run, "run");
    ("-v", Set flag_verbose, "verbose");
  ] (fun s -> filename := s ) "usage";

  let dirname = Filename.dirname !filename in
  let program = Reusable.load_program !filename in

  let dfn, cmd_list = Reusable.codegen_all dirname program in
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