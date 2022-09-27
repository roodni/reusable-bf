open OUnit2
open Named
open Support.Pervasive

(* テープの状態を比較するテスト *)

let a = Id.gen_named "a"
let p = Id.gen_named "p"
let layout =
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
let program =
  let ap_a = Sel.Array { name=a; index_opt=Some p; offset=0; member=Sel.Member a } in
  let a = Sel.Member a in
  [
    Code.Add (3, ap_a);
    Shift (1, a, p);
    Add (5, ap_a);
    Shift (1, a, p);
    Add (7, ap_a);
    Shift (-1, a, p);
    Add (-1, ap_a);
  ]

let expected = [0; 3; 1; 4; 0; 7]

let test = "shift" >:: (fun _ ->
    let bf = Codegen.gen_bf layout program in
    let _, dump, _ =
      Bf.Exe.run_string
        ~cell_type:Bf.Exe.Overflow256
        ~input:(Stream.of_string "")
        (Bf.Exe.from_code bf)
    in
    let ptr_max = List.length expected - 1 in
    let actual =
      (0 -- ptr_max) |> List.map (fun i -> Bf.Exe.Dump.geti dump i)
    in
    assert_equal
      ~printer:(fun l -> l |> List.map string_of_int |> String.concat ";")
      expected actual;
    assert_equal ~printer:string_of_int ptr_max dump.p_max
  )

let () = run_test_tt_main test