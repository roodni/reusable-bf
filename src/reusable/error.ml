open Format
open Syntax
open Support.Info

type t =
  | Lexer_Unexpected
  | Lexer_Too_large_int
  | Parser_Unexpected
  (* | File_Failed_to_read of { reason: string } *)  (* lexerがSys_errorを吐いたときに拾うやつ。open_inにも対応させるべきか？ *)
  | Syntax_Let_rec_right_hand
  | Eval_Wrong_data_type of string
  | Eval_Match_failed
  | Eval_Variable_not_defined of Var.t
  | Eval_Module_not_defined of UVar.t
  | Eval_Member_not_defined of Var.t
  | Eval_Member_is_index of Var.t
  | Eval_Member_is_not_index of Var.t
  | Eval_Equal_failed
  | Eval_Zero_division
  | Eval_Exception of string
  | Gen_Shift_interfere
  | Gen_Alloc_Array_not_implemented
  | Gen_Alloc_Index_must_be_array_member
  | Gen_Field_Unlimited_array_cannot_be_array_member
  | Gen_Field_Array_length_cannot_be_negative
  | Module_Recursive_import
  | Module_Limited_import
  | Module_import_file_not_found of string
  | Module_import_file_is_directory of string
  | Top_Missing_main
  | Top_main_is_not_stmts
  | Memory_Recursion_limit

let output ppf msg =
  let pf fs = fprintf ppf fs in
  match msg with
  | Syntax_Let_rec_right_hand -> pf "This kind of expression is not allowed as right-hand side of `let rec'"
  | Memory_Recursion_limit -> pf "Recursion depth exceeded the limit"
  | Top_Missing_main -> pf "Variable 'main' is not bound at the top level"
  | Top_main_is_not_stmts -> pf "The 'main' bound at the top level must be statements"
  | Module_Limited_import -> pf "This import path is not allowed"
  | Module_Recursive_import -> pf "This import is recursive"
  | Module_import_file_not_found path -> pf "The file \"%s\" is not found" (String.escaped path)
  | Module_import_file_is_directory path -> pf "The file \"%s\" is a directory" (String.escaped path)
  (* | File_Failed_to_read { reason } ->
      pf "The file cannot be read (%s)" reason *)
  | Gen_Field_Unlimited_array_cannot_be_array_member ->
      pf "An unlimited array cannot be allocated as a member of an array"
  | Gen_Field_Array_length_cannot_be_negative ->
      pf "The length of an array cannot be negative"
  | Gen_Alloc_Index_must_be_array_member ->
      pf "An index must be allocated as a member of an array"
  | Gen_Alloc_Array_not_implemented ->
      pf "Allocating temporary arrays is not implemented"
  | Gen_Shift_interfere ->
      pf "This shift is prohibited because an allocated member (under $dive) interferes"
  | Eval_Exception msg -> pf "Exception \"%s\"" (String.escaped msg)
  | Eval_Zero_division -> pf "A zero division is attempted"
  | Eval_Equal_failed -> pf "This equality cannot be tested"
  | Eval_Member_is_not_index v ->
      pf "Member `%s' is not an index (Use ':' instead of '@')" (Var.to_string v)
  | Eval_Member_is_index v ->
      pf "Member `%s' is an index (Use '@' instead of ':')" (Var.to_string v)
  | Eval_Variable_not_defined v ->
      pf "Variable `%s' is not bound" (Var.to_string v)
  | Eval_Module_not_defined v ->
      pf "Module `%s' is not bound" (UVar.to_string v)
  | Eval_Member_not_defined v ->
      pf "Member `%s' is not bound" (Var.to_string v)
  | Eval_Match_failed -> pf "Pattern matching failed"
  | Eval_Wrong_data_type correct ->
      pf "This value is expected to be %s" correct
  | Parser_Unexpected -> pf "This token is unexpected"
  | Lexer_Too_large_int -> pf "This integer literal is too large"
  | Lexer_Unexpected -> pf "This character is unexpected"


type exn_arg = [`Trace of trace | `Info of info option] * t
exception Exn_at of exn_arg
let at trace msg =
  raise @@ Exn_at (`Trace trace, msg)
let top info msg =
  raise @@ Exn_at (`Info (Some info), msg)
let unknown msg =
  raise @@ Exn_at (`Info None, msg)

let print ?(ppf=err_formatter) (trace, msg: exn_arg) =
  fprintf ppf "@[<v>";
  (* if lengths_of_trace trace <> [] then
    output_trace ppf trace; *)
  (match trace with
    | `Trace trace -> output_trace ppf trace;
    | `Info None -> ()
    | `Info (Some info) ->
        output_info ppf info;
        fprintf ppf "@,";
  );
  fprintf ppf "Error: @[";
  output ppf msg;
  pp_print_newline ppf ();
;;