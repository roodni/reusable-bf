open Printf
open Support.Error
open Syntax

type t = (Var.t * binded) list
and binded = (Named.Id.t * mtype) withinfo
and mtype =
  | Cell
  | Index
  | Array of {
      length: int option;
      mem: t;
    }

let to_list (t: t) = t
let lookup (v: Var.t) (t: t) = List.assoc_opt v t

let rec gen_using_field ?parent_name (nmain: Named.Field.main) nfield field =
  field |> List.fold_left
    (fun env { i=info; v=(var, mtype) } ->
      let nvar =
        Named.Id.gen_named
          ( match parent_name with
            | None -> Var.to_string var
            | Some parent_name ->
                sprintf "%s.%s" parent_name (Var.to_string var) )
      in
      match mtype with
      | Field.Cell ->
          Named.Field.extend nfield nvar (Named.Field.Cell { ifable=false });
          (var, withinfo info (nvar, Cell)) :: env
      | Field.Index ->
          Named.Field.extend nfield nvar Named.Field.Index;
          (var, withinfo info (nvar, Index)) :: env
      | Field.Array { length=Some length; mem } ->
          let nmembers = Named.Field.empty () in
          let narray = Named.Field.Array { length; members=nmembers } in
          Named.Field.extend nfield nvar narray;
          let env_members = gen_using_field nmain nmembers mem in
          (var, withinfo info (nvar, Array { length=Some length; mem=env_members })) :: env
      | Field.Array { length=None; mem } ->
          assert (nfield == nmain.finite); (* 本当はassertではダメ *)
          let env_members = gen_using_field ~parent_name:(Var.to_string var) nmain nmain.unlimited mem in
          (var,
            withinfo
              info
              (Named.Field.uarray_id, Array { length=None; mem=env_members })
          ) :: env
    ) []