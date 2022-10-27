open Printf
open Support.Pervasive
open Support.Info
open Syntax

type t = (Var.t * binded) llist
and binded = (Ir.Id.t * mtype) withinfo
and mtype =
  | Cell
  | Index
  | Array of {
      length: int option;
      mem: t;
    }

let to_llist (t: t) = t
let lookup (v: Var.t) (t: t) = LList.assoc_opt v t

let gen_using_field (nmain: Ir.Field.main) nfield field =
  let rec gen_using_field ?parent_name ~mergeable (nfield: Ir.Field.t) (field: Field.t): t =
    field
    |> LList.fold_left
      (fun (env: t) { i=info; v=(var, mtype) } : t ->
        let nvar =
          Ir.Id.gen_named
            ( match parent_name with
              | None -> Var.to_string var
              | Some parent_name ->
                  sprintf "%s/%s" parent_name (Var.to_string var) )
        in
        match mtype with
        | Field.Cell ->
            Ir.Field.extend nfield nvar (Cell { ifable=false; mergeable });
            lcons (var, withinfo info (nvar, Cell)) env
        | Field.Index ->
            if nfield == nmain.finite then
              Error.at info Gen_Alloc_Index_must_be_array_member;
            Ir.Field.extend nfield nvar Index;
            lcons (var, withinfo info (nvar, Index)) env
        | Field.Array { length=Some length; mem } ->
            let nmembers = Ir.Field.empty () in
            let narray = Ir.Field.Array { length; members=nmembers } in
            Ir.Field.extend nfield nvar narray;
            let env_members = gen_using_field ~mergeable:false nmembers mem in
            lcons (var, withinfo info (nvar, Array { length=Some length; mem=env_members })) env
        | Field.Array { length=None; mem } ->
            if nfield != nmain.finite then
              Error.at info Gen_Alloc_Unlimited_array_cannot_be_array_member;
            let env_members = gen_using_field ~parent_name:(Var.to_string var) ~mergeable:false nmain.unlimited mem in
            lcons
              (var,
                withinfo info
                  (Ir.Field.uarray_id, Array { length=None; mem=env_members })
              )
              env
      ) lnil
  in
  gen_using_field ~mergeable:true nfield field