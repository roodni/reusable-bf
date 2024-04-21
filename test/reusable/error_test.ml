open OUnit2
open Support.Info
open Reusable

let error_dir = Filename.concat (Sys.getenv "DUNE_SOURCEROOT") "examples/misc/error/compilation"

let test_error ?(path_limit=Program.NoLimit) (filename, f) =
  filename >:: fun _ ->
    let path = Filename.concat error_dir filename in
    match Program.gen_bf_from_source ~path_limit path with
    | _ -> assert_failure "No error"
    | exception Error.Exn_at e ->
        if not (f e) then begin
          Error.print ~ppf:Format.str_formatter e;
          assert_failure @@ Format.flush_str_formatter ();
        end
;;

let expe err : Error.exn_arg -> bool = function
  | `Trace _, msg -> err = msg
  | `Info _, _ -> false
let expe_len lens : Error.exn_arg -> bool = function
  | `Trace trace, _ -> lens = lengths_of_trace trace
  | `Info _, _ -> false
let expe_full lens err e =
  expe err e && expe_len lens e
let expe_notrace err : Error.exn_arg -> bool = function
  | `Trace _, _ -> false
  | `Info _, msg -> err = msg

let cases =
  let var s = Syntax.Var.of_string s in
  let uvar s = Syntax.UVar.of_string s in
  let open Error in
  [ ( "eval_equal.bfr", expe Eval_Equal_failed );
    ( "eval_failwith.bfr", expe @@ Eval_Exception "Failed" );
    ( "eval_match_fun.bfr", expe Eval_Match_failed );
    ( "eval_match_let-expr.bfr", expe Eval_Match_failed );
    ( "eval_match_let-top.bfr", expe Eval_Match_failed );
    ( "eval_match_match.bfr", expe Eval_Match_failed );
    ( "eval_nd-member_idx.bfr",
      expe @@ Eval_Member_not_defined (var "idx") );
    ( "eval_nd-member_mem.bfr",
      expe @@ Eval_Member_not_defined (var "mem") );
    ( "eval_nd-module.bfr",
      expe @@ Eval_Module_not_defined (uvar "M") );
    ( "eval_nd-var_module.bfr",
      expe @@ Eval_Variable_not_defined (var "c") );
    ( "eval_nd-var_var.bfr",
      expe @@ Eval_Variable_not_defined (var "hoge") );
    ( "eval_unexpected-index.bfr",
      expe @@ Eval_Member_is_index (var "i") );
    ( "eval_unexpected-non-index.bfr",
      expe @@ Eval_Member_is_not_index (var "x") );
    ( "eval_wdt_bool.bfr", expe @@ Eval_Wrong_data_type "bool" );
    ( "eval_wdt_function.bfr", expe @@ Eval_Wrong_data_type "function" );
    ( "eval_wdt_int.bfr", expe @@ Eval_Wrong_data_type "int" );
    ( "eval_wdt_list.bfr", expe @@ Eval_Wrong_data_type "list" );
    ( "eval_wdt_stmts.bfr", expe @@ Eval_Wrong_data_type "statements" );
    ( "eval_wdt_array.bfr", expe @@ Eval_Wrong_data_type "array selector" );
    ( "eval_wdt_cell.bfr", expe @@ Eval_Wrong_data_type "cell selector" );
    ( "eval_wdt_index.bfr", expe @@ Eval_Wrong_data_type "index selector" );
    ( "eval_wdt_selectable.bfr", expe @@ Eval_Wrong_data_type "array or index selector" );
    ( "eval_zero-div_div.bfr", expe Eval_Zero_division );
    ( "eval_zero-div_mod.bfr", expe Eval_Zero_division );
    ( "gen_alloc-index_diving.bfr", expe Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-index_root.bfr", expe Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-tmp-array.bfr", expe Gen_Alloc_Array_not_implemented );
    ( "gen_uarray-in-array.bfr", expe Gen_Field_Unlimited_array_cannot_be_array_member );
    ( "gen_negative-length-array.bfr", expe Gen_Field_Array_length_cannot_be_negative );
    ( "lexer_string.bfr", expe_notrace Lexer_Unexpected );
    ( "lexer_unexpected.bfr", expe_notrace Lexer_Unexpected );
    ( "lexer_large-int.bfr", expe_notrace Lexer_Too_large_int );
    ( "parser_1.bfr", expe_notrace Parser_Unexpected );
    ( "parser_2.bfr", expe_notrace Parser_Unexpected );
    ( "top_codegen-wrongtype.bfr", expe_notrace Top_main_is_not_stmts );
    ( "top_codegen-missing.bfr", expe_notrace Top_Missing_main );
    ( "module_import-rec_1.bfr", expe_notrace Module_Recursive_import );
    ( "module_import-not-found.bfr", expe_notrace @@ Module_import_file_not_found "./file" );
    ( "memory_stack_eval.bfr", expe Memory_Recursion_limit );
    ( "memory_stack_gen.bfr", expe Memory_Recursion_limit );
    ( "trace_eval.bfr", expe_full [1; 1] Eval_Match_failed );
    ( "trace_tail.bfr", expe_full [4; 1] @@ Eval_Exception "f" );
    ( "trace_gen.bfr", expe_len [1; 1] );
  ]

let normal_tests = "normal" >::: List.map test_error cases

let sandbox_tests =
  let open Error in
  "sandbox" >:::
    List.map
      (test_error ~path_limit:(Program.Limited ["std.bfr"]))
      [ ( "module_prohibited-import.bfr", expe_notrace Module_Limited_import );
        ( "module_prohibited-import-submodule.bfr", expe_notrace Module_Limited_import );
      ]

let too_large_bf_test = "too large bf" >:: fun _ ->
  let bfcode =
    Program.gen_bf_from_source (Filename.concat error_dir "etc_large-bfcode.bfr")
  in
  assert_equal max_int (Bf.Code.length bfcode)

let () =
  run_test_tt_main
    ("error" >::: [
      normal_tests;
      sandbox_tests;
      too_large_bf_test;
    ])