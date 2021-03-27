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
      let bf = Cmd.codegen layout program in
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
    input: string;
    output: string;
  }
  let test_run {name; dfn; cmd_list; input; output} =
    name >:: (fun _ ->
      let layout = Layout.of_dfn dfn in
      let bf = Cmd.codegen layout cmd_list in
      let res = Bf.run bf (String.enum input) in
      assert_equal ~printer:Bf.Err.opt_to_string None res.err;
      assert_equal ~printer:(fun s -> s) output (Bf.State.output_to_string res)
    )

  let cases = [
    let inc sel = Cmd.Add (1, sel) in
    let dec sel = Cmd.Add (-1, sel) in
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
          inc flag;
          Loop (flag, [
            dec flag;
            Get input; Put input;
            Add (int_of_char '#', diff);
            Loop (input, [
              dec input;
              inc els;
              Loop (diff, [
                Loop (diff, [ dec diff; inc hoge ]);
                dec els;
                dec hoge;
              ]);
              Loop (els, [
                dec els;
                Loop (input, [ dec input ]);
                inc flag;
              ]);
              Loop (hoge, [ dec hoge; inc diff ]);
            ]);
            Loop (diff, [
              Loop (diff, [ dec diff ]);
              inc flag;
            ]);
          ]);
        ]
      end;
      input = "How are you?\nI'm fine, thank you.# hoge";
      output = "How are you?\nI'm fine, thank you.#";
    }
  ]

  let tests = "output" >::: List.map test_run cases
end

let tests = "named" >::: [ OutputTest.tests; TapeTest.tests ]