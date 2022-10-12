open Syntax
open Support.Error

(** VarとIr.Varの対応 *)
type t
and binded = (Ir.Id.t * mtype) withinfo
and mtype =
  | Cell
  | Index
  | Array of {
      length: int option;
      mem: t;
    }

val to_list: t -> (Var.t * binded) list
val lookup: Var.t -> t -> binded option

(** Fieldを読んでIrのFieldを拡張してNVarEnvを作成する *)
val gen_using_field: Ir.Field.main -> Ir.Field.t -> Field.t -> t