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
  | ExLetRec of Var.t * expr * expr
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

type program = toplevel list
and toplevel = toplevel' withinfo
and toplevel' =
  | TopLet of let_binding
  | TopLetRec of Var.t * expr
  | TopOpen of mod_expr
  | TopInclude of mod_expr
  | TopModule of UVar.t * mod_expr
and mod_expr = mod_expr' withinfo
and mod_expr' =
  | ModImport of string
  | ModStruct of program
  | ModVar of UVar.t list


(* 関数 scan_*** には2つの役割がある
  1. 式の末尾が文列リテラルである展開文にフラグを立てる
  2. 位置情報に関数名・モジュール名を与える
*)

(* pnameというのは モジュール名:...:変数名 のこと *)

let rec scan_pat ~pname (pat: pat) =
  set_pname_of_info pat.i pname;
  let scan_pat = scan_pat ~pname in
  match pat.v with
  | PatVar _ | PatWild | PatInt _ | PatBool _ | PatUnit -> ();
  | PatCons (p1, p2) | PatPair (p1, p2) ->
      scan_pat p1;
      scan_pat p2;
  | PatList l ->
      List.iter scan_pat l;
;;

(** 式の末尾の形 *)
type tail_expr_kind = [`Stmts | `NonStmts ]

let rec scan_expr ~pname (expr: expr) : tail_expr_kind =
  set_pname_of_info expr.i pname;
  let scan_expr = scan_expr ~pname in
  let scan_expr_u ex = ignore @@ scan_expr ex in
  match expr.v with
  | ExVar _ | ExInt _ | ExBool _ | ExStr _ | ExUnit -> `NonStmts;
  | ExSelMem (ex, exopt, _) ->
      scan_expr_u ex;
      Option.iter scan_expr_u exopt;
      `NonStmts
  | ExSelIdx (ex, _) -> scan_expr_u ex; `NonStmts
  | ExFun (pat, ex) ->
      scan_pat ~pname pat;
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
      scan_stmts ~pname stmts;
      `Stmts
  | ExLet ((pat, ex1), ex2) ->
      scan_pat ~pname pat;
      scan_expr_u ex1;
      scan_expr ex2
  | ExLetRec (_, ex1, ex2) ->
      (* TODO: 変な右辺を禁止する *)
      scan_expr_u ex1;
      scan_expr ex2
  | ExList el ->
      List.iter scan_expr_u el; `NonStmts
  | ExMatch (e0, bindings) ->
      scan_expr_u e0;
      let tails =
        List.rev_map
          (fun (pat, ex) ->
            scan_pat ~pname pat;
            scan_expr ex )
          bindings
      in
      if List.for_all ((=) `Stmts) tails
        then `Stmts else `NonStmts
and scan_stmts ~pname (stmts: stmts) =
  let scan_stmts = scan_stmts ~pname in
  let scan_expr = scan_expr ~pname in
  let scan_expr_u ex = ignore @@ scan_expr ex in
  List.iter
    (fun st ->
      set_pname_of_info st.i pname;
      match st.v with
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
          scan_stmts stmts;
      | StDive (None, stmts) ->
          scan_stmts stmts;
      | StIf (ex, ss, ssopt) ->
          scan_expr_u ex;
          scan_stmts ss;
          Option.iter scan_stmts ssopt;
      | StAlloc (field, stmts) | StBuild (field, stmts) ->
          scan_field ~pname field;
          scan_stmts stmts;
    )
    stmts
and scan_field ~pname (field: field) =
  List.iter
    (fun { v=(_, mtype); i=info } ->
      set_pname_of_info info pname;
      match mtype with
      | MtyExCell | MtyExIndex -> ()
      | MtyExArray { mem; length; } -> begin
          (match length with
            | None -> ()
            | Some expr ->
                ignore @@ scan_expr ~pname expr;
          );
          scan_field ~pname mem;
        end
    )
    field
;;

let rec scan_module_expr ~pname mod_expr =
  set_pname_of_info mod_expr.i pname;
  match mod_expr.v with
  | ModImport _ | ModVar _ -> ()
  | ModStruct prog ->
      scan_program ~pname prog
and scan_program ~pname (prog: program) =
  List.iter
    (fun top ->
      set_pname_of_info top.i pname;
      match top.v with
      | TopLet (pat, expr) ->
          let pname = match pat.v, pname with
            | PatVar v, None -> Var.to_string v |> Option.some
            | PatVar v, Some base ->
                Printf.sprintf "%s:%s" base (Var.to_string v)
                |> Option.some
            | _ -> pname
          in
          scan_pat ~pname pat;
          ignore @@ scan_expr ~pname expr;
      | TopLetRec (v, expr) ->
          let pname = match pname with
            | None -> Var.to_string v
            | Some base -> Printf.sprintf "%s:%s" base (Var.to_string v)
          in
          let pname = Some pname in
          ignore @@ scan_expr ~pname expr;
      | TopOpen modex | TopInclude modex ->
          scan_module_expr ~pname modex
      | TopModule (uvar, modex) ->
          let pname =
            Option.some @@
            match pname with
            | None -> UVar.to_string uvar
            | Some base -> Printf.sprintf "%s:%s" base (UVar.to_string uvar)
          in
          scan_module_expr ~pname modex
    )
    prog
;;