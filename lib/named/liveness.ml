(* 生存セル解析 *)

open Support.Pervasive

module CellSet = struct
  module IdSet = Set.Make(Id)
  include IdSet

  let remove_sel sel cs = remove (Sel.last_id sel) cs
  let add_sel_if_mergeable fmain sel cs =
    match Sel.find_field fmain sel with
    | Field.Cell { mergeable=true; _ } -> add (Sel.last_id sel) cs
    | Cell { mergeable=false; _ } -> cs
    | _ -> assert false

  let to_string cs =
    let separated =
      to_seq cs
      |> Seq.map (fun id -> Id.numbered_name id)
      |> List.of_seq |> String.concat ", "
    in
    Printf.sprintf "{%s}" separated
end

type table = { mutable live_out: CellSet.t }
let create_table () = { live_out=CellSet.empty }

type code_with_liveness = table Code.t

(** 先頭のノードの入口生存セルと各ノードの出口生存セル *)
type analysis_result = CellSet.t * code_with_liveness

let analyze (fmain: Field.main) (code: 'a Code.t): analysis_result =
  let code = Code.annot_map (fun _ -> create_table ()) code in
  let rec update_tables_and_compute_live_in (succ_live_in: CellSet.t) (code: code_with_liveness): CellSet.t =
    code
    |> List.rev
    |> List.fold_left
      (fun (succ_live_in: CellSet.t) Code.{ cmd; annot=tbl }: CellSet.t ->
        (* 各コマンドに対して
           - live_out のテーブルを更新する
           - live_in を計算する
        *)
        match cmd with
        | Add (0, _) | Shift _ -> (* 解析に関係ない奴ら *)
            tbl.live_out <- succ_live_in;
            succ_live_in
        | Get sel | Reset sel -> (* def & not use *)
            tbl.live_out <- succ_live_in;
            CellSet.remove_sel sel succ_live_in
        | Add (_, sel) | Put sel -> (* use *)
            tbl.live_out <- succ_live_in;
            CellSet.add_sel_if_mergeable fmain sel succ_live_in
        | If (cond_sel, thn_code, els_code) ->
            let thn_live_in = update_tables_and_compute_live_in succ_live_in thn_code in
            let els_live_in = update_tables_and_compute_live_in succ_live_in els_code in
            tbl.live_out <- CellSet.union thn_live_in els_live_in;
            CellSet.add_sel_if_mergeable fmain cond_sel tbl.live_out
        | Loop (cond_sel, child_code) ->
            (* 出口生存に初期値を設定 *)
            tbl.live_out <- CellSet.union tbl.live_out succ_live_in;
            (* 現在の出口生存から入口生存を計算する *)
            let compute_loop_live_in () =
              CellSet.add_sel_if_mergeable fmain cond_sel tbl.live_out
            in
            (* 不動点に達するまで出口生存を更新する *)
            let rec update_until_fixed_point () =
              let loop_live_in = compute_loop_live_in () in
              let child_live_in = update_tables_and_compute_live_in loop_live_in child_code in
              let next_loop_live_out = CellSet.union succ_live_in child_live_in in
              if not (CellSet.equal tbl.live_out next_loop_live_out) then begin
                tbl.live_out <- next_loop_live_out;
                update_until_fixed_point ();
              end;
            in
            update_until_fixed_point ();
            compute_loop_live_in ()
        | LoopIndex (_, _, child_code) ->
          (* LoopIndexでは条件分岐でマージ可能なセルを使わない *)
            tbl.live_out <- CellSet.union tbl.live_out succ_live_in;
            let rec update_until_fixed_point () =
              let child_live_in = update_tables_and_compute_live_in tbl.live_out child_code in
              let next_loop_live_out = CellSet.union succ_live_in child_live_in in
              if not (CellSet.equal tbl.live_out next_loop_live_out) then begin
                tbl.live_out <- next_loop_live_out;
                update_until_fixed_point ()
              end;
            in
            update_until_fixed_point ();
            tbl.live_out
      )
      succ_live_in
  in
  let live_in = update_tables_and_compute_live_in CellSet.empty code in
  (live_in, code)

let show_analysis_result ppf (live_in, code: analysis_result) =
  let open Format in
  let rec print_code code =
    let print_block code =
      fprintf ppf "[ @[<v>";
      print_code code;
      fprintf ppf " ]@]";
    in
    List.iteri
      (fun i Code.{ cmd; annot={live_out} } ->
        let live_out_s = CellSet.to_string live_out in
        if i > 0 then fprintf ppf "@,";
        (match cmd with
        | Add (n, sel) ->
            let cmdc = if n >= 0 then '+' else '-' in
            fprintf ppf "%c %s %d\t%s"
              cmdc (Sel.to_string sel) (abs n) live_out_s;
        | Put sel ->
            fprintf ppf ". %s\t%s" (Sel.to_string sel) live_out_s;
        | Get sel ->
            fprintf ppf ", %s\t%s" (Sel.to_string sel) live_out_s;
        | Reset sel ->
            fprintf ppf "$reset %s\t%s" (Sel.to_string sel) live_out_s;
        | Shift { n; index=(arr_sel, idx_id); _ } ->
            let cmdc = if n >= 0 then '>' else '<' in
            fprintf ppf "%c %s@%s %d\t%s"
              cmdc (Sel.to_string arr_sel) (Id.simple_name idx_id) (abs n) live_out_s;
        | Loop (sel, code) ->
            fprintf ppf "! %s\t%s@;<0 2>" (Sel.to_string sel) live_out_s;
            print_block code;
        | LoopIndex (arr_sel, idx_id, code) ->
            fprintf ppf "! %s@%s\t%s@;<0 2>" (Sel.to_string arr_sel) (Id.simple_name idx_id) live_out_s;
            print_block code;
        | If (sel, thn_code, els_code) ->
            fprintf ppf "? %s\t%s@;<0 2>" (Sel.to_string sel) live_out_s;
            print_block thn_code;
            fprintf ppf "@;<0 2>";
            print_block els_code;
        );
      )
      code
  in
  fprintf ppf "@[<v>";
  fprintf ppf "$entrypoint\t%s@," (CellSet.to_string live_in);
  print_code code;
  fprintf ppf "@,@]";
;;

(** 干渉グラフ *)
module Graph = struct
  type t = {
    mutable nodes: CellSet.t;
    edges: (Id.t, CellSet.t) Hashtbl.t;
    children: (Id.t, t) Hashtbl.t
  }

  let mem_cell graph node = CellSet.mem node graph.nodes
  let succ graph node =
    assert (mem_cell graph node);
    Hashtbl.find_default graph.edges node CellSet.empty
  let add_edge graph n1 n2 =
    let n1_succ = succ graph n1 in
    let n2_succ = succ graph n2 in
    Hashtbl.replace graph.edges n1 (CellSet.add n2 n1_succ);
    Hashtbl.replace graph.edges n2 (CellSet.add n1 n2_succ);
  ;;

  (** Fieldからグラフの雛形を作成し、
      さらに セルのid -> 所属グラフ の対応表を返す
  *)
  let init Field.{ finite; unlimited }: t * (Id.t -> t option) =
    let cell_to_graph = Hashtbl.create 100 in
    let rec from_field (field: Field.t): t =
      let graph = {
        nodes = CellSet.empty;
        edges = Hashtbl.create 30;
        children = Hashtbl.create 10;
      } in
      Field.fold
        (fun (id: Id.t) (mtype: Field.mtype) (): unit ->
          match mtype with
          | Cell { mergeable=true; _ } ->
              graph.nodes <- CellSet.add id graph.nodes;
              Hashtbl.add cell_to_graph id graph;
          | Cell { mergeable=false; _ } | Index -> ()
          | Array { members; _ } ->
              Hashtbl.add graph.children id (from_field members)
        )
        field
        ();
      graph
    in
    let graph = from_field finite in
    Hashtbl.add graph.children Field.uarray_id (from_field unlimited);
    ( graph,
      (fun cell -> Hashtbl.find_opt cell_to_graph cell) )

  (** 生存セル解析の結果をもとにグラフに辺を追加する *)
  let set_edges
      (cell_to_graph: Id.t -> t option) (live_in, code: analysis_result) =
    let add_interfere def live_out =
      match cell_to_graph def with
      | None -> ()
      | Some graph ->
          CellSet.to_seq live_out
          |> Seq.filter (mem_cell graph)
          |> Seq.iter
            (fun living_cell -> add_edge graph def living_cell)
    in
    (* 先頭のノードの入口生存セル同士は干渉している *)
    CellSet.iter
      (fun c -> add_interfere c live_in)
      live_in;
    (* 各ノードの干渉を追加する *)
    let rec scan_code (code: code_with_liveness) =
      List.iter
        (fun Code.{ cmd; annot={ live_out } } ->
          match cmd with
          | Add (0, _) | Put _ | Shift _ ->
              ()
          | Add (_, sel) | Get sel | Reset sel ->
              add_interfere (Sel.last_id sel) live_out;
          | Loop (_, code) | LoopIndex (_, _, code) ->
              scan_code code;
          | If (_, thn, els) ->
              scan_code thn;
              scan_code els;
        )
        code
    in
    scan_code code
  ;;

  let create fmain analysis_result =
    let graph, cell_to_graph = init fmain in
    set_edges cell_to_graph analysis_result;
    graph

  let output_dot ppf graph =
    let open Format in
    let rec output_graph (graph: t) =
      (* ノード *)
      CellSet.iter
        (fun n ->
          fprintf ppf "%d [label=\"%s\"];@ " (Id.to_int n) (Id.numbered_name n);
        )
        graph.nodes;
      (* エッジ *)
      let rec output_edges = function
        | [] -> ()
        | n1 :: tl ->
            let n1_succ = succ graph n1 in
            List.iter
              (fun n2 ->
                if CellSet.mem n2 n1_succ then
                  fprintf ppf "%d -- %d;@ " (Id.to_int n1) (Id.to_int n2;);
              )
              tl;
            output_edges tl
      in
      CellSet.to_seq graph.nodes |> List.of_seq |> output_edges;
      (* サブグラフ *)
      Hashtbl.iter
        (fun array_id graph ->
          fprintf ppf "subgraph cluster_%d {@ " (Id.to_int array_id);
          fprintf ppf "label = \"%s\";@ " (Id.simple_name array_id);
          output_graph graph;
          fprintf ppf "};@ ";
        )
        graph.children
    in
    fprintf ppf "graph {@ ";
    fprintf ppf "graph [rankdir=LR];@ ";
    output_graph graph;
    fprintf ppf "}";
  ;;
end