open OUnit2
open Support.Info
open Reusable

let test_error (filename, f) =
  filename >:: fun _ ->
    let path = "../../sample/error/" ^ filename in
    match Program.gen_bf_from_source path with
    | _ -> assert_failure "No error"
    | exception Error.Exn_at msg_wi ->
        if not (f msg_wi.v) then begin
          Error.print ~ppf:Format.str_formatter msg_wi;
          assert_failure @@ Format.flush_str_formatter ();
        end
;;

let cases =
  let var s = Syntax.Var.of_string s in
  let uvar s = Syntax.UVar.of_string s in
  let open Error in
  [ ( "eval_equal.bfr", (=) Eval_Equal_failed );
    ( "eval_match_fun.bfr", (=) Eval_Match_failed );
    ( "eval_match_let-expr.bfr", (=) Eval_Match_failed );
    ( "eval_match_let-top.bfr", (=) Eval_Match_failed );
    ( "eval_match_match.bfr", (=) Eval_Match_failed );
    ( "eval_nd-member_idx.bfr",
      (=) @@ Eval_Member_not_defined (var "idx") );
    ( "eval_nd-member_mem.bfr",
      (=) @@ Eval_Member_not_defined (var "mem") );
    ( "eval_nd-module.bfr",
      (=) @@ Eval_Module_not_defined (uvar "M") );
    ( "eval_nd-var_module.bfr",
      (=) @@ Eval_Variable_not_defined (var "c") );
    ( "eval_nd-var_var.bfr",
      (=) @@ Eval_Variable_not_defined (var "hoge") );
    ( "eval_unexpected-index.bfr",
      (=) @@ Eval_Member_is_index (var "i") );
    ( "eval_unexpected-non-index.bfr",
      (=) @@ Eval_Member_is_not_index (var "x") );
    ( "eval_wdt_bool.bfr", (=) @@ Eval_Wrong_data_type "bool" );
    ( "eval_wdt_function.bfr", (=) @@ Eval_Wrong_data_type "function" );
    ( "eval_wdt_int.bfr", (=) @@ Eval_Wrong_data_type "int" );
    ( "eval_wdt_list.bfr", (=) @@ Eval_Wrong_data_type "list" );
    ( "eval_wdt_stmts.bfr", (=) @@ Eval_Wrong_data_type "statements" );
    ( "eval_wdt_array.bfr", (=) @@ Eval_Wrong_data_type "array selector" );
    ( "eval_wdt_cell.bfr", (=) @@ Eval_Wrong_data_type "cell selector" );
    ( "eval_wdt_index.bfr", (=) @@ Eval_Wrong_data_type "index selector" );
    ( "eval_wdt_selectable.bfr", (=) @@ Eval_Wrong_data_type "array or index selector" );
    ( "eval_zero-div_div.bfr", (=) Eval_Zero_division );
    ( "eval_zero-div_mod.bfr", (=) Eval_Zero_division );
    ( "gen_alloc-index_diving.bfr", (=) Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-index_root.bfr", (=) Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-tmp-array.bfr", (=) Gen_Alloc_Array_not_implemented );
    ( "gen_uarray-in-array.bfr", (=) Gen_Alloc_Unlimited_array_cannot_be_array_member );
    ( "lexer_string.bfr", (=) Lexer_Unexpected );
    ( "lexer_unexpected.bfr", (=) Lexer_Unexpected );
    ( "lexer_large-int.bfr", (=) Lexer_Too_large_int );
    ( "parser_1.bfr", (=) Parser_Unexpected );
    ( "parser_2.bfr", (=) Parser_Unexpected );
    ( "top_codegen-dup.bfr", (=) Top_Duplicated_codegen );
    ( "top_codegen-missing.bfr", (=) Top_Missing_codegen );
    ( "top_import-rec_1.bfr", (=) Top_Recursive_import );
    ( "memory_stack_eval.bfr", (=) Recursion_Limit );
    ( "memory_stack_gen.bfr", (=) Recursion_Limit );
  ]

let tests = "non-sandbox" >::: List.map test_error cases

let sandbox_tests =
  "sandbox" >:::
    [ ("prohibited-import_1" >:: fun _ ->
        match
          Program.gen_bf_from_source ~sandbox:true
            "../../sample/error/top_prohibited-import_normal.bfr"
        with
        | _ -> assert_failure "No error"
        | exception Error.Exn_at { i=_; v=Top_Sandbox_import } -> ()
      );
      ("prohibited-import_2" >:: fun _ ->
        match
          Program.gen_bf_from_source ~sandbox:true
            "../../sample/error/top_prohibited-import_as.bfr"
        with
        | _ -> assert_failure "No error"
        | exception Error.Exn_at { i=_; v=Top_Sandbox_import } -> ()
      );
    ]

let too_large_bf_test = "too large bf" >:: fun _ ->
  let bfcode =
    Program.gen_bf_from_source "../../sample/error/memory_bfcode.bfr"
  in
  assert_equal max_int (Bf.Code.length bfcode)

let () =
  run_test_tt_main
    ("error" >::: [
      tests;
      sandbox_tests;
      too_large_bf_test;
    ])