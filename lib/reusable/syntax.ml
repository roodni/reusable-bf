open Support.Pervasive
open Support.Info

module type VarS = sig
  type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
end

module Var : VarS = struct
  type t = string
  let compare = compare
  let of_string s = s
  let to_string s = s
end

(* 先頭が大文字の識別子 *)
module UVar : VarS = struct
  type t = string
  let compare = compare
  let of_string s = s
  let to_string s = s
end

module BOp = struct
  type op_int =
    | Add | Sub | Mul | Div | Mod
    | Lt | Leq | Gt | Geq
end

type pat = pat' withinfo
and pat' =
  | PatVar of Var.t
  | PatWild
  | PatCons of pat * pat
  | PatList of pat list
  | PatPair of pat * pat
  | PatInt of int
  | PatBool of bool
  | PatUnit

type expr = expr' withinfo
and expr' =
  | ExVar of UVar.t list * Var.t
  | ExInt of int
  | ExBool of bool
  | ExStr of string
  | ExSelMem of expr * expr option * Var.t
  | ExSelIdx of expr * Var.t
  | ExFun of pat * expr
  | ExApp of expr * expr
  | ExBlock of stmts
  | ExAnd of expr * expr
  | ExOr of expr * expr
  | ExBOpInt of expr * BOp.op_int * expr
  | ExMinus of expr
  | ExEqual of [`Eq | `Neq] * expr * expr
  | ExIf of expr * expr * expr
  | ExLet of let_binding * expr
  | ExCons of expr * expr
  | ExList of expr list
  | ExMatch of expr * (pat * expr) list
  | ExPair of expr * expr
  | ExUnit
and let_binding = pat * expr

and stmts = stmt' withinfo list
and stmt' =
  | StAdd of int * expr * expr option  (* sign, cell, int *)
  | StPut of expr
  | StGet of expr
  | StWhile of expr * stmts
  | StIf of expr * stmts * stmts option
  | StIndexLoop of expr * stmts
  | StIndexIf of expr * stmts
  | StShift of int * expr * expr option  (* sign, index, int *)
  | StAlloc of field * stmts
  | StBuild of field * stmts
  | StExpand of { ex_stmts : expr; mutable req_trace : bool; }
  | StUnit of expr
  | StDive of expr option * stmts

and field = (Var.t * mtype_expr) withinfo list
and mtype_expr =
  | MtyExCell
  | MtyExIndex
  | MtyExArray of {
      length: expr option;
      mem: field;
    }

type top_gen = stmts

type program = toplevel list
and toplevel = toplevel' withinfo
and toplevel' =
  | TopLet of let_binding
  | TopCodegen of top_gen
  | TopOpen of mod_expr
  | TopInclude of mod_expr
  | TopModule of UVar.t * mod_expr
and mod_expr = mod_expr' withinfo
and mod_expr' =
  | ModImport of string
  | ModStruct of program
  | ModVar of UVar.t list

let rec validate_pat_depth n (pat: pat) =
  if n > 10000 then failwith "too deep pattern";
  let validate_pat_depth = validate_pat_depth (n + 1) in
  match pat.v with
  | PatVar _ | PatWild | PatInt _ | PatBool _ | PatUnit -> ();
  | PatCons (p1, p2) | PatPair (p1, p2) ->
      validate_pat_depth p1;
      validate_pat_depth p2;
  | PatList l ->
      List.iter validate_pat_depth l;
;;

(** 式の末尾の形 *)
type tail_expr_kind = [`Stmts | `NonStmts ]

(* 関数 scan_*** には3つの役割がある
*)

let rec scan_expr n (expr: expr) : tail_expr_kind =
  if n > 10000 then failwith "too deep expression";
  let scan_expr = scan_expr (n + 1) in
  let scan_expr_u ex = ignore @@ scan_expr ex in
  match expr.v with
  | ExVar _ | ExInt _ | ExBool _ | ExStr _ | ExUnit -> `NonStmts;
  | ExSelMem (ex, exopt, _) ->
      scan_expr_u ex;
      Option.iter scan_expr_u exopt;
      `NonStmts
  | ExSelIdx (ex, _) -> scan_expr_u ex; `NonStmts
  | ExFun (pat, ex) ->
      validate_pat_depth 0 pat;
      scan_expr_u ex;
      `NonStmts
  | ExApp (e1, e2) | ExAnd (e1, e2) | ExOr(e1, e2)
  | ExBOpInt (e1, _, e2) | ExEqual (_, e1, e2)
  | ExCons (e1, e2) | ExPair (e1, e2) ->
      List.iter scan_expr_u [e1; e2];
      `NonStmts
  | ExMinus ex -> scan_expr_u ex; `NonStmts
  | ExIf (e1, e2, e3) ->
      scan_expr_u e1;
      let r1 = scan_expr e2 in
      let r2 = scan_expr e3 in
      if r1 = `Stmts && r2 = `Stmts then `Stmts
      else `NonStmts
  | ExBlock stmts ->
      validate_stmts_depth (n + 1) stmts;
      `Stmts
  | ExLet ((pat, ex1), ex2) ->
      validate_pat_depth 0 pat;
      scan_expr_u ex1;
      scan_expr ex2
  | ExList el ->
      List.iter scan_expr_u el; `NonStmts
  | ExMatch (e0, bindings) ->
      scan_expr_u e0;
      let tails =
        List.rev_map
          (fun (pat, ex) ->
            validate_pat_depth 0 pat;
            scan_expr ex )
          bindings
      in
      if List.for_all ((=) `Stmts) tails
        then `Stmts else `NonStmts
and validate_stmts_depth n (stmts: stmts) =
  if n > 10000 then failwith "too deep statements";
  let validate_stmts_depth = validate_stmts_depth (n + 1) in
  let scan_expr = scan_expr (n + 1) in
  let scan_expr_u ex = ignore @@ scan_expr ex in
  List.iter
    (fun st -> match st.v with
      | StAdd (_, ex, exopt) | StShift (_, ex, exopt) ->
          ignore @@ scan_expr ex;
          Option.iter scan_expr_u exopt;
      | StPut ex | StGet ex | StUnit ex ->
          scan_expr_u ex;
      | StExpand stex ->
          let tail = scan_expr stex.ex_stmts in
          if tail = `Stmts then stex.req_trace <- false;
      | StWhile (ex, stmts) | StIndexLoop (ex, stmts)
      | StDive (Some ex, stmts) | StIndexIf (ex, stmts) ->
          scan_expr_u ex;
          validate_stmts_depth stmts;
      | StDive (None, stmts) ->
          validate_stmts_depth stmts;
      | StIf (ex, ss, ssopt) ->
          scan_expr_u ex;
          validate_stmts_depth ss;
          Option.iter validate_stmts_depth ssopt;
      | StAlloc (field, stmts) | StBuild (field, stmts) ->
          validate_field_depth (n + 1) field;
          validate_stmts_depth stmts;
    )
    stmts
and validate_field_depth n (field: field) =
  if n > 10000 then failwith "too deep field";
  List.iter
    (fun { v=(_, mtype); _ } ->
      match mtype with
      | MtyExCell | MtyExIndex -> ()
      | MtyExArray { mem; length; } -> begin
          (match length with
            | None -> ()
            | Some expr ->
                ignore @@ scan_expr (n + 1) expr;
          );
          validate_field_depth (n + 1) mem;
        end
    )
    field
;;

let rec validate_mod_expr_depth n mod_expr =
  if n > 10000 then failwith "too deep modules";
  match mod_expr.v with
  | ModImport _ | ModVar _ -> ()
  | ModStruct prog ->
      validate_program_depth (n + 1) prog
and validate_program_depth n (prog: program) =
  List.iter
    (fun top -> match top.v with
      | TopLet (pat, expr) ->
          validate_pat_depth 0 pat;
          ignore @@ scan_expr 0 expr;
      | TopCodegen stmts ->
          validate_stmts_depth 0 stmts;
      | TopOpen modex | TopInclude modex | TopModule (_, modex) ->
          validate_mod_expr_depth n modex
    )
    prog
;;