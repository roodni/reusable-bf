open Syntax
open Support.Error

(** VarとNamed.Varの対応 *)
type t
and binded = (Named.Id.t * mtype) withinfo
and mtype =
  | Cell
  | Index
  | Array of {
      length: int option;
      mem: t;
    }

val to_list: t -> (Var.t * binded) list
val lookup: Var.t -> t -> binded option

(** Fieldを読んでNamedのFieldを拡張してNVarEnvを作成する *)
val gen_using_field: Named.Field.main -> Named.Field.t -> Field.t -> t