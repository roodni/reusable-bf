open OUnit2
open Printf

open Support.Info

let output_test =
  let test_run Testcase.{ path; io_list; cell_type; _ } =
    path >:: (fun _ ->
      let field, ir_code =
        try
          Reusable.Program.load_from_source path
          |> Result.get_ok
          |> Reusable.Program.gen_ir
            ~path_limit:NoLimit
            (Filename.dirname path)
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
      let dir = Filename.concat Testcase.source_root "examples/misc/error/execution" in
      let path = Filename.concat dir name in
      let program = Reusable.Program.load_from_source path |> Result.get_ok in
      let field, ir_code =
        Reusable.Program.gen_ir
          ~path_limit:NoLimit
          dir
          program
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
      ("eof.bfr", 5, "End of input");
      ("index1.bfr", 9, "Index out of range");
      ("index2.bfr", 10, "Index out of range");
      ("index3.bfr", 11, "Index out of range");
      ("overflow1.bfr", 6, "Overflow");
      ("overflow2.bfr", 5, "Overflow");
    ]


let tests = "iri" >::: [output_test; error_test]
let () = run_test_tt_main tests