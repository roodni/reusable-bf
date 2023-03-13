(* 生存セル解析 *)

open Support.Pervasive


module IdSet = Set.Make(Id)
module IdMap = Map.Make(Id)

module CellSet = struct
  include IdSet

  let remove_sel sel cs = remove (Sel.last_id sel) cs
  let add_sel_if_sticky fmain sel cs =
    match Sel.find_mtype fmain sel with
    | Field.Cell { sticky=true; _ } -> add (Sel.last_id sel) cs
    | Cell { sticky=false; _ } -> cs
    | _ -> assert false

  let to_string cs =
    to_seq cs |> Seq.map Id.number_only_name
    |> List.of_seq |> String.concat ", "
    |> Printf.sprintf "{%s}"
end

type table = { mutable live_out: CellSet.t }
let create_table () = { live_out=CellSet.empty }

type code_with_liveness = table Code.t

(** 先頭のノードの入口生存セルと各ノードの出口生存セル *)
type analysis_result = CellSet.t * code_with_liveness

let analyze (fmain: Field.main) (code: 'a Code.t): analysis_result =
  let code = Code.annot_map (fun _ -> create_table ()) code in
  let rec update_tables_and_compute_live_in
      (succ_live_in: CellSet.t) (code: code_with_liveness) : CellSet.t =
    code
    |> LList.rev
    |> LList.fold_left
      (fun (succ_live_in: CellSet.t) Code.{ cmd; annot=tbl; _ } : CellSet.t ->
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
            CellSet.add_sel_if_sticky fmain sel succ_live_in
        | If (cond_sel, thn_code, els_code) ->
            let thn_live_in = update_tables_and_compute_live_in succ_live_in thn_code in
            let els_live_in = update_tables_and_compute_live_in succ_live_in els_code in
            tbl.live_out <- CellSet.union thn_live_in els_live_in;
            CellSet.add_sel_if_sticky fmain cond_sel tbl.live_out
        | IndexIf (_, thn_code) ->
            let thn_live_in = update_tables_and_compute_live_in succ_live_in thn_code in
            tbl.live_out <- CellSet.union thn_live_in succ_live_in;
            tbl.live_out
        | Loop (cond_sel, child_code) ->
            (* 出口生存に初期値を設定 *)
            tbl.live_out <- CellSet.union tbl.live_out succ_live_in;
            (* 現在の出口生存から入口生存を計算する *)
            let compute_loop_live_in () =
              CellSet.add_sel_if_sticky fmain cond_sel tbl.live_out
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
        | IndexLoop (_, child_code) ->
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

let output_analysis_result ppf (live_in, code: analysis_result) =
  let open Format in
  fprintf ppf "@[<v>";
  fprintf ppf "$entrypoint\t%s@," (CellSet.to_string live_in);
  Code.output ppf
    (fun ppf { live_out } ->
      fprintf ppf "\t%s" (CellSet.to_string live_out); )
    code;
  fprintf ppf "@]";
;;

(** 干渉グラフ *)
module Graph : sig
  type t
  val create: Field.main -> analysis_result -> t
  val output_dot: Format.formatter -> t -> unit
  val create_program_with_merged_cells: t -> Field.main -> 'a Code.t -> Field.main * unit Code.t
end = struct
  type t = {
    mutable nodes: CellSet.t;
    edges: (Id.t, CellSet.t) Hashtbl.t;
    children: (Id.t, t) Hashtbl.t;
    mutable colored_groups_memo: Id.t list list option;
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
        edges = Hashtbl.create 100;
        children = Hashtbl.create 10;
        colored_groups_memo = None;
      } in
      (* ノードの登録 *)
      Field.fold
        (fun (id: Id.t) (mtype: Field.mtype) (): unit ->
          match mtype with
          | Cell { sticky=true; _ } ->
              graph.nodes <- CellSet.add id graph.nodes;
              Hashtbl.add cell_to_graph id graph;
          | Cell { sticky=false; _ } | Index -> ()
          | Array { members; _ } ->
              Hashtbl.add graph.children id (from_field members)
        )
        field ();
      (* 異なるインデックス下のstickyセルは干渉する *)
      let idx_to_cells, no_idx_cells =
      Field.fold
        (fun id mtype (idx_to_cells, no_idx_cells) ->
          match mtype with
          | Cell { sticky=true; idx_id; _ } -> begin
              match idx_id with
              | None -> (idx_to_cells, id :: no_idx_cells)
              | Some idx_id ->
                  let l =
                    IdMap.find_opt idx_id idx_to_cells
                    |> Option.value ~default:[]
                  in
                  (IdMap.add idx_id (id :: l) idx_to_cells, no_idx_cells)
            end
          | Cell { sticky=false; _ } | Index | Array _ ->
              (idx_to_cells, no_idx_cells)
        )
        field (IdMap.empty, [])
      in
      let cells_by_id =
        no_idx_cells ::
        (IdMap.bindings idx_to_cells |> List.map snd)
      in
      let rec add_interferes = function
        | [] -> ()
        | cells :: rest ->
            cells |> List.iter (fun cell ->
              rest |> List.iter (fun other_cells ->
                other_cells |> List.iter (fun other_cell ->
                  add_edge graph cell other_cell
                )
              )
            );
            add_interferes rest
      in
      add_interferes cells_by_id;
      (* グラフを返す *)
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
      LList.iter
        (fun Code.{ cmd; annot={ live_out }; _ } ->
          match cmd with
          | Add (0, _) | Put _ | Shift _ ->
              ()
          | Add (_, sel) | Get sel | Reset sel ->
              add_interfere (Sel.last_id sel) live_out;
          | Loop (_, code) | IndexLoop (_, code) ->
              scan_code code;
          | If (_, thn, els) ->
              scan_code thn;
              scan_code els;
          | IndexIf (_, thn) ->
              scan_code thn;
        )
        code
    in
    scan_code code
  ;;

  (** グラフ彩色を行いセルを色で分類して返す *)
  let coloring g =
    match g.colored_groups_memo with
    | Some groups -> groups (* 彩色済み *)
    | None -> (* 未彩色 *)
        let cell_to_color = Hashtbl.create (CellSet.cardinal g.nodes) in
        (* 次数の大きい順に並べる *)
        let degree_ordered =
          CellSet.elements g.nodes
          |> List.sort
            (fun n1 n2 ->
              -Int.compare
                (CellSet.cardinal (succ g n1))
                (CellSet.cardinal (succ g n2)) )
        in
        (* 貪欲彩色 *)
        let color_num = ref 0 in
        List.iter
          (fun cell ->
            let rec try_color i =
              if !color_num <= i then color_num := i + 1;
              assert (!color_num < 200000);
              let possible =
                CellSet.for_all
                  (fun succ_cell -> Hashtbl.find_opt cell_to_color succ_cell <> Some i)
                  (succ g cell)
              in
              if possible
                then Hashtbl.add cell_to_color cell i
                else try_color (i + 1)
            in
            try_color 0
          )
          degree_ordered;
        (* 色でグループ化して返す *)
        let color_to_cells = Array.make !color_num [] in
        CellSet.iter
          (fun cell ->
            let color = Hashtbl.find cell_to_color cell in
            color_to_cells.(color) <- cell :: color_to_cells.(color)
          )
          g.nodes;
        let groups = Array.to_list color_to_cells |> List.map List.rev in
        g.colored_groups_memo <- Some groups;
        groups

  let create fmain analysis_result =
    let graph, cell_to_graph = init fmain in
    set_edges cell_to_graph analysis_result;
    graph

  let output_dot ppf graph =
    let open Format in
    let rec output_graph (graph: t) =
      (* ノード *)
      let colored_groups = coloring graph in
      let output_node n =
        fprintf ppf "%d [label=\"%s\"];@ " (Id.to_int n) (Id.numbered_name n);
      in
      List.iter
        (function
          | [] -> assert false
          | [cell] ->
              output_node cell;
          | (hd :: _) as group ->
              fprintf ppf "subgraph cluster_%d {@ " (Id.to_int hd);
              fprintf ppf "label = \"\";@ ";
              List.iter output_node group;
              fprintf ppf "};@ ";
        )
        colored_groups;
      (* エッジ *)
      CellSet.iter
        (fun n1 ->
          CellSet.iter
            (fun n2 ->
              if Id.compare n1 n2 < 0 then
                fprintf ppf "%d -- %d;@ " (Id.to_int n1) (Id.to_int n2;);
            )
            (succ graph n1);
        )
        graph.nodes;
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

  (** 彩色に従ってセルを結合したフィールドとコードを返す *)
  let create_program_with_merged_cells (g: t) (fmain: Field.main) (code: 'a Code.t) =
    (* 結合されたセルのidとそれを持つフィールドを同時に生成する *)
    let id_convert_tbl = Hashtbl.create 100 in
    let rec create_mc_field (g: t) (field: Field.t): Field.t =
      let mc_field = Field.empty () in
      (* 同色のセルを結合して登録 *)
      let colored_groups = coloring g in
      List.iter
        (fun (group: Id.t list) ->
          let merged_id = Id.gen_merged group in
          let merged_idx_id =
            match Field.lookup field (List.hd group) with
            | Cell { idx_id; _ } -> idx_id
            | Array _ | Index -> assert false
          in
          let merged_ifable = ref false in
          List.iter
            (fun id ->
              (match Field.lookup field id with
              | Cell { ifable; idx_id; _ } ->
                  if ifable then merged_ifable := true;
                  assert (merged_idx_id = idx_id);
              | Array _ | Index -> assert false
              );
              Hashtbl.add id_convert_tbl id merged_id;
            )
            group;
          Field.extend mc_field
            merged_id
            (Field.Cell {
              ifable=(!merged_ifable);
              sticky=true;
              idx_id=merged_idx_id;
            });
        )
        colored_groups;
      (* その他のメンバの転記 *)
      Field.fold
        (fun (id: Id.t) (mtype: Field.mtype) (): unit ->
          (match mtype with
          | Cell { sticky=true; _ } ->
              ()
          | Cell { sticky=false; _ } | Index ->
              Field.extend mc_field id mtype;
          | Array { length; members } ->
              let members_graph = Hashtbl.find g.children id in
              let members = create_mc_field members_graph members in
              Field.extend mc_field id (Array { length; members })
          );
        )
        field
        ();
      mc_field
    in
    let mc_fmain = Field.{
      finite = create_mc_field g fmain.finite;
      unlimited = create_mc_field (Hashtbl.find g.children Field.uarray_id) fmain.unlimited;
      (* XXX: unlimited arrayの干渉グラフをfiniteのグラフの干渉グラフのサブグラフにするのをやめる *)
    } in
    let id_to_mc id =
      match Hashtbl.find_opt id_convert_tbl id with
      | Some id -> id
      | None -> id
    in
    let convert_sel = Sel.convert_id id_to_mc in
    let convert_index (arr_sel, idx_id) = (convert_sel arr_sel, id_to_mc idx_id) in
    (* コードのId書き換え *)
    let rec convert_code (code: 'a Code.t): unit Code.t =
      let open Code in
      LList.map
        (fun { cmd; info; _ } ->
          let cmd =
            match cmd with
            | Add (n, sel) -> Add (n, convert_sel sel)
            | Put sel -> Put (convert_sel sel)
            | Get sel -> Get (convert_sel sel)
            | Reset sel -> Reset (convert_sel sel)
            | Shift { n; index=(sel, id); followers } ->
                Shift {
                  n;
                  index = (convert_sel sel, id_to_mc id);
                  followers =
                    LList.to_seq followers
                    |> Seq.map id_to_mc
                    |> CellSet.of_seq |> CellSet.elements |> llist
                }
            | Loop (sel, code) -> Loop (convert_sel sel, convert_code code)
            | IndexLoop (index, code) ->  IndexLoop (convert_index index, convert_code code)
            | If (sel, thn, els) -> If (convert_sel sel, convert_code thn, convert_code els)
            | IndexIf (index, thn) -> IndexIf (convert_index index, convert_code thn)
          in
          { cmd; annot=(); info }
        )
        code
    in
    (mc_fmain, convert_code code)
end