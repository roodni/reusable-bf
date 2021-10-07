open Support.Error

module Var : sig
  type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string
  let compare = compare
  let of_string s = s
  let to_string s = s
end

module VMap = Map.Make(Var)

(* 先頭が大文字の識別子 *)
module UVar : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string
  let of_string s = s
  let to_string s = s
end

module Field = struct
  type t = (Var.t * mtype) withinfo list
  and mtype =
    | Cell
    | Index
    | Array of {
        length : int option;
        mem : t;
      }
end

module BOpInt = struct
  type t = Add | Sub | Mul | Div | Mod | Lt | Leq
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

type stmt = stmt' withinfo
and stmt' =
  | StAdd of int * expr * expr option  (* sign, sel, int *)
  | StPut of expr
  | StGet of expr
  | StWhile of expr * stmt list
  | StIf of expr * stmt list * stmt list option
  | StShift of int * expr * expr option  (* sign, ptr, int *)
  | StVar of Field.t * stmt list
  | StLet of let_binding * stmt list
  | StExpand of expr
  | StDive of expr * stmt list
and expr = expr' withinfo
and expr' =
  | ExVar of Var.t
  | ExInt of int
  | ExBool of bool
  | ExStr of string
  | ExSelMem of expr * expr option * Var.t
  | ExSelPtr of expr * Var.t
  | ExFun of pat * expr
  | ExApp of expr * expr
  | ExBlock of stmt list
  | ExBOpInt of expr * BOpInt.t * expr
  | ExMinus of expr
  | ExEqual of expr * expr
  | ExIf of expr * expr * expr
  | ExLet of let_binding * expr
  | ExNil
  | ExCons of expr * expr
  | ExList of expr list
  | ExMatch of expr * (pat * expr) list
  | ExPair of expr * expr
and let_binding = pat * expr

type toplevel' =
  | TopLet of let_binding
  | TopImport of string
  | TopImportAs of string * UVar.t
type toplevel = toplevel' withinfo
type main = Field.t * stmt list
type program = toplevel list * main option