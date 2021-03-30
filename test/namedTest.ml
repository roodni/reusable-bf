open Batteries
open OUnit2
open Bfhoge
open Bfhoge.Named

(* テープの状態を比較するテスト *)
module TapeTest = struct
  let a = Var.gen_named "a"
  let p = Var.gen_named "p"
  let layout =
    let open Layout in [
      (a, {
        offset = 1;
        kind = Lst {
          mem = [
            (a, { offset = 0; kind = Cell });
            (p, { offset = 1; kind = Ptr });
          ];
          header_start = -1;
          elm_size = 2;
        }
      });
    ]
  let program =
    let ap_a = Sel.LstPtr (a, p, 0, Sel.Cell a) in
    let a = Sel.Cell a in
    let open Cmd in [
      Add (3, ap_a);
      Shift (1, a, p);
      Add (5, ap_a);
      Shift (1, a, p);
      Add (7, ap_a);
      Shift (-1, a, p);
      Add (-1, ap_a);
    ]
  
  let expected = [0; 0; 3; 1; 4; 0; 7]

  let tests = "shift" >:: (fun _ ->
      let bf = codegen layout program in
      let state = Bf.run bf (Enum.empty ()) in
      let Bf.State.{ tape; _ } = state in
      let ptr_max = List.length expected - 1 in
      let actual = 
        (0 -- ptr_max) |> map (fun i -> Bf.Tape.geti i tape) |> List.of_enum
      in
      assert_equal
        ~printer:(fun l -> l |> List.map string_of_int |> String.concat ";")
        expected actual;
      assert_equal ~printer:string_of_int ptr_max tape.ptr_max
    )

end


(* 出力結果を比較するテスト *)
module OutputTest = struct
  type case = {
    name: string;
    dfn: Dfn.t;
    cmd_list: Cmd.t_list;
    io_list: (string * string) list
  }
  let test_run {name; dfn; cmd_list; io_list} =
    name >:: (fun _ ->
      let layout = Layout.of_dfn dfn in
      let bf = codegen layout cmd_list in
      io_list |> List.iter (fun (ipt, opt) ->
        let state = Bf.run bf (String.enum ipt) in
        let Bf.State.{ err; _ } = state in
        if err != None then begin
          bf |> Bf.Cmd.list_to_string |> print_endline;
          Bf.State.dump state;
          assert_bool "err" false;
        end;
        assert_equal ~printer:(fun s -> s) opt (Bf.State.output_to_string state);
      )
    )

  let cases = 
    let svar v = Sel.Cell v in
    let slst ?p ?(i=0) lst sel =
      match p with
      | None -> Sel.Lst (lst, i, sel)
      | Some p -> Sel.LstPtr (lst, p, i, sel)
    in
    let add ?(n=1) s = Cmd.Add (n, s) in
    let sub ?(n=1) s = Cmd.Add (-n, s) in
    let loop s l = Cmd.Loop (s, l) in
    let add2 ?(times=1) s_from s_dest =
      loop s_from [
        sub s_from;
        add ~n:times s_dest;
      ]
    in
    let sub2 ?(times=1) s_from s_dest =
      loop s_from [
        sub s_from;
        sub ~n:times s_dest
      ]
    in
    let del s = loop s [ sub s ] in
    [
      begin
        (* echo *)
        let var_list = (1--5) |> Enum.map (fun _ -> Var.gen ()) |> List.of_enum in
        {
          name = "echo";
          dfn = var_list |> List.map (fun v -> (v, Dfn.Cell));
          cmd_list = begin
            let flag, input, diff, hoge, els =
              match var_list |> List.map (fun v -> Sel.Cell v) with
              | [a; b; c; d; e] -> (a, b, c, d, e)
              | _ -> assert false
            in
            let open Cmd in [
              add flag;
              Loop (flag, [
                sub flag;
                Get input; Put input;
                Add (int_of_char '#', diff);
                Loop (input, [
                  sub input;
                  add els;
                  Loop (diff, [
                    Loop (diff, [ sub diff; add hoge ]);
                    sub els;
                    sub hoge;
                  ]);
                  Loop (els, [
                    sub els;
                    Loop (input, [ sub input ]);
                    add flag;
                  ]);
                  Loop (hoge, [ sub hoge; add diff ]);
                ]);
                Loop (diff, [
                  Loop (diff, [ sub diff ]);
                  add flag;
                ]);
              ]);
            ]
          end;
          io_list = [
            ( "How are you?\nI'm fine, thank you.# hoge",
              "How are you?\nI'm fine, thank you.#" )
          ];
        }
      end; begin
        (* garden https://atcoder.jp/contests/abc106/tasks/abc106_a *)
        let ipt = Var.gen_named "ipt" in
        let tmp = Var.gen_named "tmp" in
        let a = Var.gen_named "a" in
        let b = Var.gen_named "b" in
        let arr = Var.gen_named "arr" in
        let n = Var.gen_named "n" in
        let up = Var.gen_named "up" in
        let flg = Var.gen_named "flg" in
        let p = Var.gen_named "p" in
        {
          name = "garden";
          dfn = begin
            let open Dfn in
            of_list [
              (ipt, Cell);
              (tmp, Cell);
              (a, Cell);
              (b, Cell);
              (arr, Lst {
                length = Some 4;
                mem = of_list [
                  (n, Cell);
                  (up, Cell);
                  (tmp, Cell);
                  (p, Ptr);
                  (flg, Cell);
                ];
              })
            ]
          end;
          cmd_list = begin
            let open Cmd in
            let ipt_base10 ed ~dest ~tmp:(ipt, tmp) =
              [
                add ipt;
                loop ipt [
                  Get ipt;
                  sub ~n:(int_of_char ed) ipt;
                  add tmp;
                  (* if *)
                  loop ipt [
                    sub tmp;
                    add2 dest tmp;
                    sub ~n:(int_of_char '0' - int_of_char ed) ipt;
                    add2 ipt dest;
                    add2 ~times:10 tmp dest;
                  ];
                  add ipt;
                  (* else *)
                  loop tmp [ sub tmp;
                    sub ipt;
                  ];
                ];
              ]
            in
            ipt_base10 ' ' ~dest:(svar a) ~tmp:(svar ipt, svar tmp) @
            ipt_base10 '\n' ~dest:(svar b) ~tmp:(svar ipt, svar tmp) @
            [
              sub (svar a);
              sub (svar b);
              loop (svar a) [ sub (svar a);
                loop (svar b) [ sub (svar b); add (svar tmp);
                  (* Comment "loop"; *)
                  (* Dump; *)
                  add (slst ~i:0 arr @@ svar up);
                  loop (slst ~p arr @@ svar up) begin
                    let p_up = (slst ~p arr @@ svar up) in
                    let p_n = (slst ~p arr @@ svar n) in
                    let p_tmp = (slst ~p arr @@ svar tmp) in
                    let p_flg = (slst ~p arr @@ svar flg) in
                    [
                      add ~n:9 p_tmp;
                      sub2 p_n p_tmp;
                      loop p_tmp [
                        sub p_up;
                        add ~n:10 p_n;
                        sub2 p_tmp p_n;
                      ];
                      loop p_up [
                        sub p_up;
                        add (slst ~p ~i:1 arr @@ svar up);
                      ];
                      loop p_flg [ sub p_flg ]; add p_flg;
                      Shift (1, svar arr, p);
                    ]
                  end;
                  loop (slst ~p ~i:(-1) arr @@ svar p) [
                    Shift (-1, svar arr, p);
                  ];
                ];
                add2 (svar tmp) (svar b);
              ];
              loop (slst ~p arr @@ svar flg) [
                Shift (1, svar arr, p);
              ];
              LoopPtr (svar arr, p, [
                Shift (-1, svar arr, p);
                add ~n:(int_of_char '0') (slst ~p arr @@ svar n);
                Put (slst ~p arr @@ svar n);
              ]);
            ]
          end;
          io_list = [
            ("2 2\n", "1");
            ("5 7\n", "24");
            ("12 34\n", "363");
            ("99 99\n", "9604");
          ];
        }
      end; begin
        (* unlimited list *)
        let buf1 = Var.gen () in
        let buf2 = Var.gen () in
        let c = Var.gen () in
        let p = Var.gen () in
        {
          name = "unlimited list";
          dfn = begin
            let open Dfn in [
              (c, Cell);
              (buf1, Lst {
                length = None;
                mem = [
                  (c, Cell);
                  (p, Ptr);
                ]
              });
              (buf2, Lst {
                length = None;
                mem = [
                  (c, Cell);
                  (p, Ptr);
                ]
              });
            ]
          end;
          cmd_list = [
            Comment "input1";
            add (slst ~i:0 buf1 @@ svar c);
            loop (slst ~p buf1 @@ svar c) [
              Shift (1, svar buf1, p);
              Get (slst ~p buf1 @@ svar c);
              sub ~n:10 (slst ~p buf1 @@ svar c);
            ];
            sub (slst ~i:0 buf1 @@ svar c);
            
            Comment "input2";
            add (slst ~i:0 buf2 @@ svar c);
            loop (slst ~p buf2 @@ svar c) [
              Shift (1, svar buf2, p);
              Get (slst ~p buf2 @@ svar c);
              sub ~n:10 (slst ~p buf2 @@ svar c);
            ];
            sub (slst ~i:0 buf2 @@ svar c);

            Comment "output1";
            Shift (-1, svar buf1, p);
            loop (slst ~p buf1 @@ svar c) [
              add ~n:10  (slst ~p buf1 @@ svar c);
              Put (slst ~p buf1 @@ svar c);
              Shift (-1, svar buf1, p);
            ];

            add ~n:(int_of_char ' ') (svar c);
            Put (svar c);

            Comment "output2";
            Shift (-1, svar buf2, p);
            loop (slst ~p buf2 @@ svar c) [
              add ~n:10  (slst ~p buf2 @@ svar c);
              Put (slst ~p buf2 @@ svar c);
              Shift (-1, svar buf2, p);
            ];
          ];
          io_list = [ ("hello\ntest\n", "olleh tset") ]
        }
      end; begin
        (* echo2 *)
        let flag = Var.gen_named "named" in
        let ipt = Var.gen_named "ipt" in
        let diff = Var.gen_named "diff" in
        {
          name = "echo2 (if)";
          dfn = begin
            let open Dfn in [
              (flag, Cell);
              (ipt, CellIfable);
              (diff, Cell);
            ]
          end;
          cmd_list = begin
            let open Cmd in [
              add (svar flag);
              loop (svar flag) [
                sub (svar flag);
                Get (svar ipt);
                Put (svar ipt);
                add ~n:(int_of_char '#') (svar diff);
                loop (svar diff) [
                  sub (svar diff);
                  If (svar ipt, [
                    sub (svar ipt);
                  ], [
                    del (svar diff);
                    add (svar flag);
                  ]);
                ];
                If (svar ipt, [
                  add (svar flag);
                ], []);
              ];
            ]
          end;
          io_list = [
            ( "How are you?\nI'm fine, thank you.# hoge",
              "How are you?\nI'm fine, thank you.#" )
          ]
        }
      end;
    ]

  let tests = "output" >::: List.map test_run cases
end

let tests = "named" >::: [ OutputTest.tests; TapeTest.tests ]