open Support.Pervasive

let gen_bf (layout: Layout.t) (code: 'a Code.t): Bf.Code.t =
  let code = Code.delete_annot code in
  let rec gen_bf (pos_init: Pos.t) (code: unit Code.t) =
    let pos, bf_code_list =
      List.fold_left_map
        (fun pos Code.{ cmd; trace; _ }: (Pos.t * Bf.Code.t) ->
          match cmd with
          | Add (0, _) | Use _ -> (pos, [])
          | Add (n, sel) ->
              let pos_dest = Pos.from_sel_of_cell layout sel in
              (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Add n ])
          | Put sel ->
              let pos_dest = Pos.from_sel_of_cell layout sel in
              (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Put ])
          | Get sel ->
              let pos_dest = Pos.from_sel_of_cell layout sel in
              (pos_dest, Pos.gen_bf_move pos pos_dest @ [ Bf.Code.Get ])
          | Loop (sel, code) ->
              let pos_cond = Pos.from_sel_of_cell layout sel in
              let bf_move1 = Pos.gen_bf_move pos pos_cond in
              let pos, bf_loop = gen_bf pos_cond code in
              let bf_move2 = Pos.gen_bf_move pos pos_cond in
              (pos_cond, bf_move1 @ [ Bf.Code.Loop (bf_loop @ bf_move2) ])
          | IndexLoop params ->
              gen_bf pos (Code.extend_IndexLoop trace params)
          | IndexIf params ->
              gen_bf pos (Code.extend_IndexIf trace params)
          | Shift { n; index; followers } -> begin
              let idx_id = snd index in
              (* let pos_ptr =
                  Sel.concat_member_to_index_tail index idx_id 0
                  |> Pos.from_sel_of_cell layout
              and pos_ptr_prev =
                Sel.concat_member_to_index_tail index idx_id (-1)
                |> Pos.from_sel_of_cell layout
              in *)
              match n with
              | 0 -> (pos, [])
              (* | 1 ->
                  let pos, bf_shift_followers =
                    gen_bf pos (Code.shift_followers trace 1 index followers)
                  in
                  let bf_move = Pos.gen_bf_move pos pos_ptr in
                  let bf =
                    bf_shift_followers @ bf_move @ [ Bf.Code.Add 1 ]
                  in
                  (pos_ptr_prev, bf)
              | -1 ->
                  let pos, bf_shift_followers =
                    gen_bf pos (Code.shift_followers trace (-1) index followers)
                  in
                  let bf_move = Pos.gen_bf_move pos pos_ptr_prev in
                  let bf =
                    bf_shift_followers @ bf_move @ [ Bf.Code.Add (-1) ]
                  in
                  (pos_ptr, bf) *)
              | n ->
                  (* 全展開とループの二通りの方法が考えられるが、実装が容易かつ高速な全展開を選択 *)
                  let pos, bf_shift_followers =
                    gen_bf pos (Code.shift_followers trace n index followers)
                  in
                  let _, bf_codes =
                    List.init (abs n) Fun.id
                    |> List.fold_left_map
                        (fun pos i ->
                          let pos' =
                            Sel.concat_member_to_index_tail index idx_id
                              (if n > 0 then i else -1 - i)
                            |> Pos.from_sel_of_cell layout
                          in
                          let bf =
                            Pos.gen_bf_move pos pos'
                            @ [Bf.Code.Add (if n > 0 then 1 else -1)]
                          in
                          (pos', bf)
                        )
                        pos
                  in
                  let final_pos =
                    Sel.concat_member_to_index_tail index idx_id
                      (if n > 0 then -1 else 0)
                    |> Pos.from_sel_of_cell layout
                  in
                  (final_pos, bf_shift_followers @ List.flatten bf_codes)
            end
          | If (cond, code_then, code_else) ->
              let ifable = Pos.from_sel_of_ifable layout cond in
              let pos_then_end, bf_then = gen_bf ifable.pos_else code_then in
              let pos_else_end, bf_else = gen_bf ifable.pos_else code_else in
              let bf =
                Pos.gen_bf_move pos ifable.pos_else @
                [ Bf.Code.Add 1;
                  Bf.Code.Shift (-ifable.cond_to_else);
                  Bf.Code.Loop (
                    [ Bf.Code.Shift ifable.cond_to_else;
                      Bf.Code.Add (-1);
                    ] @
                    bf_then @
                    Pos.gen_bf_move pos_then_end ifable.pos_prev_endif
                  );
                  Bf.Code.Shift ifable.cond_to_else;
                  Bf.Code.Loop (
                    [ Bf.Code.Add (-1) ] @
                    bf_else @
                    Pos.gen_bf_move pos_else_end ifable.pos_endif
                  );
                ]
              in
              (ifable.pos_endif, bf)
          | Reset sel ->
              [ Code.Loop
                (sel, [ Code.Add (-1, sel) ] |> Code.from_cmds trace)
              ] |> Code.from_cmds trace
              |> gen_bf pos
        )
        pos_init
        code
    in
    (pos,
    bf_code_list |> List.concat)
  in
  let _, bf = gen_bf Pos.init code in
  bf