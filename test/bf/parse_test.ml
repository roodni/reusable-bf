open OUnit2
open Support.Pervasive
open Bf

let test = "parse" >::: [
  "add shift" >:: (fun _ ->
      let s = "+++ +-+>>>*><>" in
      let bf = Code.parse (Stream.of_string s) in
      assert_equal
        (llist [ Code.Add 4; Shift 4 ])
        bf
    );
  "lbracket" >:: (fun _ ->
      assert_raises
        Code.ParseError
        (fun () -> Code.parse (Stream.of_string "["))
    );
  "lbracket" >:: (fun _ ->
      assert_raises
        Code.ParseError
        (fun () -> Code.parse (Stream.of_string "]"))
    );
]

let () = run_test_tt_main test