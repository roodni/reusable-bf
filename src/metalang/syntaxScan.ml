open Info
open Syntax

(* 関数 scan_*** にはいくつか役割がある
  1. 式の末尾が文列リテラルである展開文にフラグを立てる
  2. 位置情報に関数名・モジュール名を与える
  3. let rec の変な右辺を弾く
*)

(* pnameというのは モジュール名:...:変数名 のこと *)

let rec scan_pat ~pname (pat: pat) =
  set_pname_of_info pat.i pname;
  let scan_pat = scan_pat ~pname in
  match pat.v with
  | PatVar _ | PatWild | PatInt _ | PatBool _ | PatUnit -> ();
  | PatCons (p1, p2) ->
      scan_pat p1;
      scan_pat p2;
  | PatList l | PatTuple l ->
      List.iter scan_pat l;
;;

(** 式の末尾の形 *)
type tail_expr_kind = [`Stmts | `NonStmts ]

(** let recの変な右辺を弾く

  面倒なので関数直書き以外は全部禁止する
  参考: https://v2.ocaml.org/manual/letrecvalues.html
*)
let validate_let_rec_righthand ex =
  match ex.v with
  | ExFun _ | ExFunction _ | ExBlock _ -> ()
  | _ -> Error.top ex.i Syntax_Let_rec_right_hand

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
  | ExFunction clauses ->
      List.iter (fun (pat, ex) ->
          scan_pat ~pname pat;
          scan_expr_u ex;
        ) clauses;
      `NonStmts
  | ExApp (e1, e2) | ExAnd (e1, e2) | ExOr(e1, e2)
  | ExBOpInt (e1, _, e2) | ExEqual (_, e1, e2)
  | ExCons (e1, e2) ->
      List.iter scan_expr_u [e1; e2];
      `NonStmts
  | ExMinus ex -> scan_expr_u ex; `NonStmts
  | ExIf (e1, e2, e3) ->
      scan_expr_u e1;
      let r1 = scan_expr e2 in
      let r2 = scan_expr e3 in
      if r1 = `Stmts && r2 = `Stmts then `Stmts
      else `NonStmts
  | ExIfUnit (e1, e2) ->
      scan_expr_u e1;
      scan_expr_u e2;
      `NonStmts
  | ExBlock stmts ->
      scan_stmts ~pname stmts;
      `Stmts
  | ExLet ((pat, ex1), ex2) ->
      scan_pat ~pname pat;
      scan_expr_u ex1;
      scan_expr ex2
  | ExLetRec (_, ex1, ex2) ->
      (* 先にscanしてpnameを浸透させる *)
      scan_expr_u ex1;
      validate_let_rec_righthand ex1;
      scan_expr ex2
  | ExList el | ExTuple el ->
      List.iter scan_expr_u el;
      `NonStmts
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
  | ExSemicolon l ->
      List.fold_left (fun _ -> scan_expr) `NonStmts l

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
    (fun decl ->
      set_pname_of_info decl.i pname;
      match decl.v with
      | DeclExpr expr ->
          ignore @@ scan_expr ~pname expr;
      | DeclLet { binding=(pat, expr); is_priv=_ } ->
          let pname = match pat.v, pname with
            | PatVar v, None -> Var.to_string v |> Option.some
            | PatVar v, Some base ->
                Printf.sprintf "%s:%s" base (Var.to_string v)
                |> Option.some
            | _ -> pname
          in
          scan_pat ~pname pat;
          ignore @@ scan_expr ~pname expr;
      | DeclLetRec { binding=(v, expr); is_priv=_ } ->
          let pname = match pname with
            | None -> Var.to_string v
            | Some base -> Printf.sprintf "%s:%s" base (Var.to_string v)
          in
          let pname = Some pname in
          ignore @@ scan_expr ~pname expr;
          validate_let_rec_righthand expr;
      | DeclOpen modex | DeclInclude modex ->
          scan_module_expr ~pname modex
      | DeclModule { binding=(uvar, modex); is_priv=_ } ->
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