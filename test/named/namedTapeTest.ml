open Batteries
open OUnit2
open Lib
open Lib.Named

(* テープの状態を比較するテスト *)

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
  let ap_a = Sel.LstPtr (a, p, 0, Sel.V a) in
  let a = Sel.V a in
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

let test = "shift" >:: (fun _ ->
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

let () = run_test_tt_main test