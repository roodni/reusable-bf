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

let expe_trace err : Error.exn_arg -> bool = function
  | `Trace _, msg -> err = msg
  | `Info _, _ -> false
let expe_trace_len lens : Error.exn_arg -> bool = function
  | `Trace trace, _ -> lens = lengths_of_trace trace
  | `Info _, _ -> false
let expe_trace_full lens err e =
  expe_trace err e && expe_trace_len lens e
let expe_info err : Error.exn_arg -> bool = function
  | `Trace _, _ -> false
  | `Info _, msg -> err = msg
let expe_info_lc lines cols err : Error.exn_arg -> bool = function
  | `Info (Some i), msg ->
      msg = err &&
      lines_of_info i = lines &&
      cols_of_info i = cols
  | _ -> false


let cases: (string * (Error.exn_arg -> bool)) list =
  let var s = Syntax.Var.of_string s in
  let uvar s = Syntax.UVar.of_string s in
  let open Error in
  [
    ( "eval_equal.bfml", expe_trace Eval_Equal_failed );
    ( "eval_failwith.bfml", expe_trace @@ Eval_Exception "Failed" );
    ( "eval_match_fun.bfml", expe_trace Eval_Match_failed );
    ( "eval_match_let-expr.bfml", expe_trace Eval_Match_failed );
    ( "eval_match_let-decl.bfml", expe_trace Eval_Match_failed );
    ( "eval_match_match.bfml", expe_trace Eval_Match_failed );
    ( "eval_nd-member_idx.bfml",
      expe_trace @@ Eval_Member_not_defined (var "idx") );
    ( "eval_nd-member_mem.bfml",
      expe_trace @@ Eval_Member_not_defined (var "mem") );
    ( "eval_nd-module.bfml",
      expe_trace @@ Eval_Module_not_defined (uvar "M") );
    ( "eval_nd-var_module.bfml",
      expe_trace @@ Eval_Variable_not_defined (var "c") );
    ( "eval_nd-var_var.bfml",
      expe_trace @@ Eval_Variable_not_defined (var "hoge") );
    ( "eval_unexpected-index.bfml",
      expe_trace @@ Eval_Member_is_index (var "i") );
    ( "eval_unexpected-non-index.bfml",
      expe_trace @@ Eval_Member_is_not_index (var "x") );
    ( "eval_wdt_bool.bfml", expe_trace @@ Eval_Wrong_data_type "bool" );
    ( "eval_wdt_function.bfml", expe_trace @@ Eval_Wrong_data_type "function" );
    ( "eval_wdt_int.bfml", expe_trace @@ Eval_Wrong_data_type "int" );
    ( "eval_wdt_list.bfml", expe_trace @@ Eval_Wrong_data_type "list" );
    ( "eval_wdt_stmts.bfml", expe_trace @@ Eval_Wrong_data_type "statements" );
    ( "eval_wdt_array.bfml", expe_trace @@ Eval_Wrong_data_type "array selector" );
    ( "eval_wdt_cell.bfml", expe_trace @@ Eval_Wrong_data_type "cell selector" );
    ( "eval_wdt_index.bfml", expe_trace @@ Eval_Wrong_data_type "index selector" );
    ( "eval_wdt_selectable.bfml", expe_trace @@ Eval_Wrong_data_type "array or index selector" );
    ( "eval_wdt_unit.bfml", expe_trace @@ Eval_Wrong_data_type "unit" );
    ( "eval_zero-div_div.bfml", expe_trace Eval_Zero_division );
    ( "eval_zero-div_mod.bfml", expe_trace Eval_Zero_division );
    ( "gen_alloc-index_diving.bfml", expe_trace Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-index_root.bfml", expe_trace Gen_Alloc_Index_must_be_array_member );
    ( "gen_alloc-tmp-array.bfml", expe_trace Gen_Alloc_Array_not_implemented );
    ( "gen_uarray-in-array.bfml", expe_trace Gen_Field_Unlimited_array_cannot_be_array_member );
    ( "gen_negative-length-array.bfml", expe_trace Gen_Field_Array_length_cannot_be_negative );
    ( "lexer_string.bfml", expe_info Lexer_Unexpected );
    ( "lexer_unexpected.bfml", expe_info Lexer_Unexpected );
    ( "lexer_large-int.bfml", expe_info Lexer_Too_large_int );
    ( "parser_1.bfml", expe_info_lc (3, 3) (6, 7) Parser_Unexpected );
    ( "parser_2.bfml", expe_info_lc (2, 4) (5, 4) Parser_Unexpected );
    ( "top_codegen-wrongtype.bfml", expe_info Top_main_is_not_stmts );
    ( "top_codegen-missing.bfml", expe_info Top_Missing_main );
    ( "module_import-rec_1.bfml", expe_info Module_Recursive_import );
    ( "module_import-not-found.bfml", expe_info @@ Module_import_file_not_found "./file" );
    ( "module_import-not-found-absolute.bfml", expe_info @@ Module_import_file_not_found "/absolute-path-that-does-not-exist");
    ( "module_import-file-is-directory.bfml", function
        | (`Info _, File_Failed_to_read _) -> true
        | _ -> false
    );
    ( "memory_stack_eval.bfml", expe_trace Memory_Recursion_limit );
    ( "memory_stack_gen.bfml", expe_trace Memory_Recursion_limit );
    ( "trace_eval.bfml", expe_trace_full [1; 1] Eval_Match_failed );
    ( "trace_tail.bfml", expe_trace_full [4; 1] @@ Eval_Exception "f" );
    ( "trace_gen.bfml", expe_trace_len [1; 1] );
    ( "syntax_letrec.bfml", expe_info Syntax_Let_rec_right_hand );
    ( "syntax_letrec_decl.bfml", expe_info Syntax_Let_rec_right_hand );

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