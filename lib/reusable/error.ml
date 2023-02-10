open Format
open Syntax
open Support.Info

type t =
  | Lexer_Unexpected
  | Lexer_Too_large_int
  | Parser_Unexpected
  | Eval_Wrong_data_type of string
  | Eval_Match_failed
  | Eval_Variable_not_defined of Var.t
  | Eval_Module_not_defined of UVar.t
  | Eval_Member_not_defined of Var.t
  | Eval_Member_is_index of Var.t
  | Eval_Member_is_not_index of Var.t
  | Eval_Equal_failed
  | Eval_Zero_division
  | Gen_Shift_interfere
  | Gen_Alloc_Array_not_implemented
  | Gen_Alloc_Index_must_be_array_member
  | Gen_Field_Unlimited_array_cannot_be_array_member
  | Gen_Field_Array_length_cannot_be_negative
  | Module_Recursive_import
  | Module_Limited_import
  | Module_import_file_not_found of string
  (* | Module_import_failed_to_read of string *)
  | Top_Duplicated_codegen
  | Top_Missing_codegen
  | Memory_Recursion_limit

let output ppf msg =
  let pf fs = fprintf ppf fs in
  match msg with
  | Memory_Recursion_limit -> pf "Recursion depth exceeded the limit"
  | Top_Missing_codegen -> pf "There is no codegen declaration"
  | Top_Duplicated_codegen -> pf "The codegen declaration is duplicated"
  | Module_Limited_import -> pf "The import path is not allowed"
  | Module_Recursive_import -> pf "The import is recursive"
  | Module_import_file_not_found path -> pf "The file '%s' is not found" path
  (* | Module_import_failed_to_read path -> pf "The file '%s' cannot be read" path *)
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
  | Eval_Zero_division -> pf "A zero division is attempted"
  | Eval_Equal_failed -> pf "The equality cannot be tested"
  | Eval_Member_is_not_index v ->
      pf "Member '%s' is not an index (Use ':' instead of '@')" (Var.to_string v)
  | Eval_Member_is_index v ->
      pf "Member '%s' is an index (Use '@' instead of ':')" (Var.to_string v)
  | Eval_Variable_not_defined v ->
      pf "Variable '%s' is not defined" (Var.to_string v)
  | Eval_Module_not_defined v ->
      pf "Module '%s' is not defined" (UVar.to_string v)
  | Eval_Member_not_defined v ->
      pf "Member '%s' is not defined" (Var.to_string v)
  | Eval_Match_failed -> pf "Pattern matching failed"
  | Eval_Wrong_data_type correct ->
      pf "The data type of the expression value is expected to be %s" correct
  | Parser_Unexpected -> pf "The token is unexpected"
  | Lexer_Too_large_int -> pf "The integer literal is too large"
  | Lexer_Unexpected -> pf "The character is unexpected"


exception Exn_at of t withinfo
let at info msg =
  raise @@ Exn_at (withinfo info msg)

let print ?(ppf=err_formatter) (msg_wi : t withinfo) =
  let { i; v=msg } = msg_wi in
  fprintf ppf "@[<v>";
  output_info ppf i;
  fprintf ppf "  Error: @[";
  output ppf msg;
  pp_print_newline ppf ();
;;