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

let gen_using_field (nmain: Named.Field.main) nfield field =
  let rec gen_using_field ?parent_name ~mergeable (nfield: Named.Field.t) (field: Field.t): t =
    field
    |> List.fold_left
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
            Named.Field.extend nfield nvar (Cell { ifable=false; mergeable });
            (var, withinfo info (nvar, Cell)) :: env
        | Field.Index ->
            Named.Field.extend nfield nvar Index;
            (var, withinfo info (nvar, Index)) :: env
        | Field.Array { length=Some length; mem } ->
            let nmembers = Named.Field.empty () in
            let narray = Named.Field.Array { length; members=nmembers } in
            Named.Field.extend nfield nvar narray;
            let env_members = gen_using_field ~mergeable:false nmembers mem in
            (var, withinfo info (nvar, Array { length=Some length; mem=env_members })) :: env
        | Field.Array { length=None; mem } ->
            (* 無限配列を配列のメンバとして確保しようとするとエラー。
              TODO: assertではだめ。適切なエラーメッセージを表示する。
            *)
            assert (nfield == nmain.finite);
            let env_members = gen_using_field ~parent_name:(Var.to_string var) ~mergeable:false nmain.unlimited mem in
            (var,
              withinfo
                info
                (Named.Field.uarray_id, Array { length=None; mem=env_members })
            ) :: env
      ) []
  in
  gen_using_field ~mergeable:true nfield field