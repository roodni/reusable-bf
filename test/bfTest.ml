open Batteries
open OUnit2
open Bfhoge.Bf

type case = {
  name: string;
  program: Cmd.t list;
  input: string;
  output: string;
  err: Err.t option
}

let test_run { name; program; input; output; err } =
  name >:: fun _ ->
    let input = input |> String.enum in
    let res = run program input in
    assert_equal ~printer:Err.opt_to_string err res.err;
    assert_equal ~printer:(fun s -> s) output (State.output_to_string res)

let cases = [
  {
    name = "echo";
    program = [
      Add 1;
      Loop [
        Add (-1);
        Move 1; Get; Put;
        Move 1; Add (int_of_char '#');
        Move (-1); Loop [
          Add (-1);
          Move 3; Add 1;
          Move (-2); Loop [
            Loop [
              Add (-1);
              Move 1; Add 1;
              Move (-1)
            ];
            Move 2; Add (-1);
            Move (-1); Add (-1);
            Move (-1);
          ];
          Move 2; Loop [
            Add (-1);
            Move (-3); Loop [ Add (-1) ];
            Move (-1); Add 1;
            Move 4;
          ];  
          Move (-1); Loop [
            Add (-1);
            Move (-1); Add 1;
            Move 1;
          ];
          Move (-2);
        ];
        Move 1; Loop [
          Loop  [ Add (-1) ];
          Move (-2); Add 1;
          Move 2;
        ];
        Move (-2);
      ]
    ];
    input = "How are you?\nI'm fine, thank you.# hoge";
    output = "How are you?\nI'm fine, thank you.#";
    err = None
  };
  {
    name = "overflow";
    program = [ Add (-1) ];
    input = "";
    output = "";
    err = Some Overflow
  };
  {
    name = "end of input";
    program = [ Add 1; Loop [ Get; Put ] ];
    input = "abc";
    output = "abc";
    err = Some End_of_input
  };
  {
    name = "pointer out of range";
    program = [ Move (-1) ];
    input = "";
    output = "";
    err = Some Ptr_out_of_range
  }
]

let tests =
  "bf" >::: List.map test_run cases