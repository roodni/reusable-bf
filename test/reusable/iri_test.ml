open OUnit2
open Printf
open Support.Info
module Testcase = Helper.Testcase

let lib_dirs = Cli.default_lib_dirs Sys.getenv_opt

let output_test =
  let test_run Testcase.{ path; io_list; cell_type; _ } =
    path >:: (fun _ ->
      let field, ir_code =
        try
          Reusable.Program.load_from_source path
          |> Reusable.Program.gen_ir ~lib_dirs ~base_dir:(Filename.dirname path)
        with Reusable.Error.Exn_at msg ->
          Reusable.Error.print ~ppf:Format.str_formatter msg;
          assert_failure @@ Format.flush_str_formatter ();
      in
      List.iter
        (fun (ipt, opt_expe) ->
          let res, opt_act =
            Ir.Interpreter.run_string
              ~cell_type
              ~input:ipt
              field ir_code
          in
          assert_equal ~printer:Fun.id opt_expe opt_act;
          assert_bool "error" (Result.is_ok res)
        )
        io_list;
    )
  in
  "output" >::: List.map test_run Testcase.cases

let error_test =
  let test_run (name, line_expe, msg_expe) =
    name >:: (fun _ ->
      let base_dir = Filename.concat Testcase.source_root "examples/misc/error/execution" in
      let path = Filename.concat base_dir name in
      let program = Reusable.Program.load_from_source path in
      let field, ir_code =
        Reusable.Program.gen_ir ~lib_dirs ~base_dir program
      in
      let res, _ =
        Ir.Interpreter.run_string
          ~cell_type:Overflow256
          ~input:""
          field ir_code
      in
      match res with
      | Ok () -> assert_failure "Did not raise error"
      | Error (trace, msg_act) ->
          assert_equal ~printer:Fun.id msg_expe msg_act;
          assert_equal
            ~printer:(fun (a, b) -> sprintf "%d-%d" a b)
            (line_expe, line_expe)
            (top_of_trace trace |> lines_of_info)
    )
  in
  "error" >:::
    List.map test_run [
      ("eof.bfml", 5, "End of input");
      ("index1.bfml", 9, "Index out of range");
      ("index2.bfml", 10, "Index out of range");
      ("index3.bfml", 11, "Index out of range");
      ("overflow1.bfml", 6, "Overflow");
      ("overflow2.bfml", 5, "Overflow");
    ]


let tests = "iri" >::: [output_test; error_test]
let () = run_test_tt_main tests