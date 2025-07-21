open OUnit2
open Info
open Metalang

let error_dir = Filename.concat (Sys.getenv "DUNE_SOURCEROOT") "examples/misc/error/compilation"

let test_error (filename, f) =
  filename >:: fun _ ->
    let path = Filename.concat error_dir filename in
    match Helper.gen_bf_from_source path with
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
  [
    ( "eval_equal.bfml", expe Eval_Equal_failed );
    ( "eval_failwith.bfml", expe @@ Eval_Exception "Failed" );
    ( "eval_match_fun.bfml", expe Eval_Match_failed );
    ( "eval_match_let-expr.bfml", expe Eval_Match_failed );
    ( "eval_match_let-decl.bfml", expe Eval_Match_failed );
    ( "eval_match_match.bfml", expe Eval_Match_failed );
    ( "eval_nd-member_idx.bfml",
      expe @@ Eval_Member_not_defined (var "idx") );
    ( "eval_nd-member_mem.bfml",
      expe @@ Eval_Member_not_defined (var "mem") );
    ( "eval_nd-module.bfml",
      expe @@ Eval_Module_not_defined (uvar "M") );
    ( "eval_nd-var_module.bfml",
      expe @@ Eval_Variable_not_defined (var "c") );
    ( "eval_nd-var_var.bfml",
      expe @@ Eval_Variable_not_defined (var "hoge") );
    ( "eval_unexpected-index.bfml",
      expe @@ Eval_Member_is_index (var "i") );
    ( "eval_unexpected-non-index.bfml",
      expe @@ Eval_Member_is_not_index (var "x") );
    ( "eval_wdt_bool.bfml", expe @@ Eval_Wrong_data_type "bool" );
    ( "eval_wdt_function.bfml", expe @@ Eval_Wrong_data_type "function" );
    ( "eval_wdt_int.bfml", expe @@ Eval_Wrong_data_type "int" );
    ( "eval_wdt_list.bfml", expe @@ Eval_Wrong_data_type "list" );
    ( "eval_wdt_stmts.bfml", expe @@ Eval_Wrong_data_type "statements" );
    ( "eval_wdt_array.bfml", expe @@ Eval_Wrong_data_type "array selector" );
    ( "eval_wdt_cell.bfml", expe @@ Eval_Wrong_data_type "cell selector" );
    ( "eval_wdt_index.bfml", expe @@ Eval_Wrong_data_type "index selector" );
    ( "eval_wdt_selectable.bfml", expe @@ Eval_Wrong_data_type "array or index selector" );
    ( "eval_wdt_unit.bfml", expe @@ Eval_Wrong_data_type "unit" );
    ( "eval_zero-div_div.bfml", expe Eval_Zero_division );
    ( "eval_zero-div_mod.bfml", expe Eval_Zero_division );
    ( "gen_alloc-index_diving.bfml", expe Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-index_root.bfml", expe Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-tmp-array.bfml", expe Gen_Alloc_Array_not_implemented );
    ( "gen_uarray-in-array.bfml", expe Gen_Field_Unlimited_array_cannot_be_array_member );
    ( "gen_negative-length-array.bfml", expe Gen_Field_Array_length_cannot_be_negative );
    ( "lexer_string.bfml", expe_notrace Lexer_Unexpected );
    ( "lexer_unexpected.bfml", expe_notrace Lexer_Unexpected );
    ( "lexer_large-int.bfml", expe_notrace Lexer_Too_large_int );
    ( "parser_1.bfml", expe_notrace Parser_Unexpected );
    ( "parser_2.bfml", expe_notrace Parser_Unexpected );
    ( "top_codegen-wrongtype.bfml", expe_notrace Top_main_is_not_stmts );
    ( "top_codegen-missing.bfml", expe_notrace Top_Missing_main );
    ( "module_import-rec_1.bfml", expe_notrace Module_Recursive_import );
    ( "module_import-not-found.bfml", expe_notrace @@ Module_import_file_not_found "./file" );
    ( "module_import-not-found-absolute.bfml", expe_notrace @@ Module_import_file_not_found "/absolute-path-that-does-not-exist");
    ( "module_import-file-is-directory.bfml", function
        | (`Info _, File_Failed_to_read _) -> true
        | _ -> false
    );
    ( "memory_stack_eval.bfml", expe Memory_Recursion_limit );
    ( "memory_stack_gen.bfml", expe Memory_Recursion_limit );
    ( "trace_eval.bfml", expe_full [1; 1] Eval_Match_failed );
    ( "trace_tail.bfml", expe_full [4; 1] @@ Eval_Exception "f" );
    ( "trace_gen.bfml", expe_len [1; 1] );
    ( "syntax_letrec.bfml", expe_notrace Syntax_Let_rec_right_hand );
    ( "syntax_letrec_decl.bfml", expe_notrace Syntax_Let_rec_right_hand );

  ]

let normal_tests = "normal" >::: List.map test_error cases


let too_large_bf_test = "too large bf" >:: fun _ ->
  let bfcode =
    Helper.gen_bf_from_source (Filename.concat error_dir "etc_large-bfcode.bfml")
  in
  assert_equal max_int (Bf.Code.length bfcode)

let () =
  run_test_tt_main
    ("error" >::: [
      normal_tests;
      too_large_bf_test;
    ])