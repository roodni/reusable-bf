open Batteries
open OUnit2
open Lib
open Reusable
open TestLib

let test_run ReusableCases.{ name; code; io_list; } =
  name >:: (fun _ ->
    let program = Lexing.from_string (code ()) |> Parser.program Lexer.main in
    let dfn, cmd_list = Codegen.codegen program in
    let layout = Named.Layout.of_dfn dfn in
    let bf_code = Named.codegen layout cmd_list in
    io_list |> List.iter (fun (ipt, opt) ->
      let state = Bf.run bf_code (String.enum ipt) in
      let Bf.State.{ err; _ } = state in
      if err <> None || opt <> Bf.State.output_to_string state then begin
        print_endline @@ Bf.Cmd.list_to_string bf_code;
        print_endline "--- expected output ---";
        print_endline opt;
        print_newline ();
        Bf.State.dump state;
        assert_bool "fail" false
      end
    )
  )



let test = "reusable" >::: List.map test_run ReusableCases.cases

let () = run_test_tt_main test