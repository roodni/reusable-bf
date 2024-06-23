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
  | PatTuple of pat list
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
  | ExFunction of (pat * expr) list
  | ExApp of expr * expr
  | ExBlock of stmts
  | ExAnd of expr * expr
  | ExOr of expr * expr
  | ExBOpInt of expr * BOp.op_int * expr
  | ExMinus of expr
  | ExEqual of [`Eq | `Neq] * expr * expr
  | ExIf of expr * expr * expr
  | ExIfUnit of expr * expr
  | ExLet of let_binding * expr
  | ExLetRec of Var.t * expr * expr
  | ExCons of expr * expr
  | ExList of expr list
  | ExMatch of expr * (pat * expr) list
  | ExTuple of expr list
  | ExUnit
  | ExSemicolon of expr list
and let_binding = pat * expr

and stmts = stmt' withinfo list
and stmt' =
  | StAdd of int * expr * expr option  (* sign, cell, int *) (* コメントを書くならレコードを使ってほしいな *)
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

type program = decl list
and decl = decl' withinfo
and decl' =
  | DeclLet of { binding: let_binding; is_priv: bool }
  | DeclLetRec of { binding: Var.t * expr; is_priv: bool }
  | DeclOpen of mod_expr
  | DeclInclude of mod_expr
  | DeclModule of { binding: UVar.t * mod_expr; is_priv: bool }
  | DeclExpr of expr  (* 式を評価するだけ *)
and mod_expr = mod_expr' withinfo
and mod_expr' =
  | ModImport of string
  | ModStruct of program
  | ModVar of UVar.t list
