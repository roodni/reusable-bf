open OUnit2
open Bf

let test = "parse" >::: [
  "add shift" >:: (fun _ ->
      let s = "+++ +-+>>>*><>" in
      let bf = Code.parse (String.to_seq s) in
      assert_equal
        [ Code.Add 4; Shift 4 ]
        bf
    );
  "lbracket" >:: (fun _ ->
      assert_raises
        Code.ParseError
        (fun () -> Code.parse (String.to_seq "["))
    );
  "lbracket" >:: (fun _ ->
      assert_raises
        Code.ParseError
        (fun () -> Code.parse (String.to_seq "]"))
    );
]

let () = run_test_tt_main test