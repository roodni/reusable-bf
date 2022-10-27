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

module Field = struct
  type t = (Var.t * mtype) withinfo llist
  and mtype =
    | Cell
    | Index
    | Array of {
        length : int option;
        mem : t;
      }

  let rec validate_depth n (field: t) =
    if n > 100 then failwith "Too deep field";
    let validate_depth = validate_depth (n + 1) in
    LList.iter
      (fun { i=_; v=(_, mtype) } ->
        match mtype with
        | Cell | Index -> ();
        | Array { mem; length=_; } ->
            validate_depth mem;
      )
      field;
  ;;
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
  | PatNil
  | PatPair of pat * pat
  | PatInt of int
  | PatBool of bool

type expr = expr' withinfo
and expr' =
  | ExVar of Var.t
  | ExModuleVar of UVar.t * Var.t
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
  | ExList of expr llist
  | ExMatch of expr * (pat * expr) llist
  | ExPair of expr * expr
and let_binding = pat * expr

and stmts = stmt' withinfo llist
and stmt' =
  | StAdd of int * expr * expr option  (* sign, cell, int *)
  | StPut of expr
  | StGet of expr
  | StWhile of expr * stmts
  | StIf of expr * stmts * stmts option
  | StILoop of expr * stmts
  | StShift of int * expr * expr option  (* sign, index, int *)
  | StAlloc of Field.t * stmts
  | StBuild of Field.t * stmts
  | StExpand of expr
  | StDive of expr * stmts

type toplevel = toplevel' withinfo
and toplevel' =
  | TopLet of let_binding
  | TopCodegen of top_gen
  | TopImport of string
  | TopImportAs of string * UVar.t
and top_gen = stmts

type program = toplevel llist


let rec validate_pat_depth n (pat: pat) =
  if n > 10000 then failwith "too deep pattern";
  let validate_pat_depth = validate_pat_depth (n + 1) in
  match pat.v with
  | PatVar _ | PatWild | PatNil | PatInt _ | PatBool _ -> ();
  | PatCons (p1, p2) | PatPair (p1, p2) ->
      validate_pat_depth p1;
      validate_pat_depth p2;
;;

let rec validate_expr_depth n (expr: expr) =
  if n > 10000 then failwith "too deep expression";
  let validate_expr_depth = validate_expr_depth (n + 1) in
  match expr.v with
  | ExVar _ | ExModuleVar _
  | ExInt _ | ExBool _ | ExStr _ -> ();
  | ExSelMem (ex, exopt, _) ->
      validate_expr_depth ex;
      Option.iter validate_expr_depth exopt;
  | ExSelIdx (ex, _) -> validate_expr_depth ex;
  | ExFun (pat, ex) ->
      validate_pat_depth 0 pat;
      validate_expr_depth ex;
  | ExApp (e1, e2) | ExAnd (e1, e2) | ExOr(e1, e2)
  | ExBOpInt (e1, _, e2) | ExEqual (_, e1, e2)
  | ExCons (e1, e2) | ExPair (e1, e2) ->
      List.iter validate_expr_depth [e1; e2];
  | ExMinus ex -> validate_expr_depth ex;
  | ExIf (e1, e2, e3) ->
      List.iter validate_expr_depth [e1; e2; e3];
  | ExBlock stmts ->
      validate_stmts_depth (n + 1) stmts;
  | ExLet ((pat, ex1), ex2) ->
      validate_pat_depth 0 pat;
      List.iter validate_expr_depth [ex1; ex2];
  | ExList el -> LList.iter validate_expr_depth el;
  | ExMatch (e0, bindings) ->
      validate_expr_depth e0;
      LList.iter
        (fun (pat, ex) ->
          validate_pat_depth 0 pat;
          validate_expr_depth ex; )
        bindings;
and validate_stmts_depth n (stmts: stmts) =
  if n > 10000 then failwith "too deep statements";
  let validate_stmts_depth = validate_stmts_depth (n + 1) in
  let validate_expr_depth = validate_expr_depth (n + 1) in
  LList.iter
    (fun st -> match st.v with
      | StAdd (_, ex, exopt) | StShift (_, ex, exopt) ->
          validate_expr_depth ex;
          Option.iter validate_expr_depth exopt;
      | StPut ex | StGet ex | StExpand ex ->
          validate_expr_depth ex;
      | StWhile (ex, stmts) | StILoop (ex, stmts) | StDive (ex, stmts) ->
          validate_expr_depth ex;
          validate_stmts_depth stmts;
      | StIf (ex, ss, ssopt) ->
          validate_expr_depth ex;
          validate_stmts_depth ss;
          Option.iter validate_stmts_depth ssopt;
      | StAlloc (field, stmts) | StBuild (field, stmts) ->
          Field.validate_depth 0 field;
          validate_stmts_depth stmts;
    )
    stmts
;;