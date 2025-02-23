open OUnit2
open Ir
open Info

(* テープの状態を比較するテスト *)

let a = Id.gen_named "a" None
let p = Id.gen_named "p" None
let layout : Layout.t =
  let open Layout in [
    ( a,
      Array {
        offset_of_body = 1;
        size_of_members = 2;
        members = [
          (a, Cell { offset=0; is_index=false });
          (p, Cell { offset=1; is_index=false });
        ];
        length = None;
      } );
  ]

let ap_a0 = Sel.Array { name=a; index_opt=None; offset=0; member=Sel.Member a }
let ap_a = Sel.Array { name=a; index_opt=Some p; offset=0; member=Sel.Member a }
let a = Sel.Member a

let program1 =
  [ Code.Add (3, ap_a);
    Shift { n=1; index=(a, p); followers=[] };
    Add (5, ap_a);
    Shift { n=1; index=(a, p); followers=[] };
    Add (7, ap_a);
    Shift { n=(-1); index=(a, p); followers=[] };
    Add (-1, ap_a);
  ] |> Code.from_cmds empty_trace
let expected1 = [0; 3; 1; 4; 0; 7]

let program2 =
  [ Code.Shift { n=4; index=(a, p); followers=[] };
    Add (10, ap_a);
  ] |> Code.from_cmds empty_trace
let expected2 = [0; 0;1; 0;1; 0;1; 0;1; 10]

let program3 =
  [ Code.Shift { n=4; index=(a, p); followers=[] };
    Add (10, ap_a);
    Add (100, ap_a0);
    Shift { n=(-2); index=(a, p); followers=[] };
    Add (20, ap_a)
  ] |> Code.from_cmds empty_trace
let expected3 = [0; 100;1; 0;1; 20;0; 0;0; 10; 0]

let test_run (name, program, expected) =
  name >:: (fun _ ->
    let bf = BfGen.gen_bf layout program in
    let _, dump, _ =
      Bf.Exe.run_string
        ~cell_type:Bf.Exe.Overflow256
        ~input:""
        (Bf.Exe.from_code bf)
    in
    let ptr_max = List.length expected - 1 in
    let actual =
      List.init (ptr_max + 1) Fun.id
        |> List.map (fun i -> Bf.Exe.Dump.geti dump i)
    in
    assert_equal
      ~printer:(fun l -> l |> List.map string_of_int |> String.concat ";")
      expected actual;
    assert_equal ~printer:string_of_int ptr_max dump.p_max
  )

let tests = "tape" >:::
  List.map test_run
    [ ("shift1", program1, expected1);
      ("shift2", program2, expected2);
      ("shift3", program3, expected3);
    ]

let () = run_test_tt_main tests