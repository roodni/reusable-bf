open OUnit2
open Support.Pervasive
open Bf
open Code

type case = {
  name: string;
  program: Code.t;
  input: string;
  output: string;
  is_ok: bool;
  ptr_max: int;
}

let test_run { name; program; input; output; is_ok; ptr_max } =
  name >:: fun _ ->
    let res, dump, output_actual =
      Exe.run_string
        ~cell_type:Exe.Overflow256
        ~input:(Stream.of_string input)
        (Exe.from_code program)
    in
    assert_equal (Code.length program) (Code.to_buffer program |> Buffer.length);
    assert_equal ~printer:string_of_bool is_ok (Result.is_ok res);
    assert_equal ~printer:Fun.id output output_actual;
    assert_equal ~printer:string_of_int ptr_max dump.p_max

let cases = [
  {
    name = "echo";
    program = ~~[
      Add 1;
      Loop ~~[
        Add (-1);
        Shift 1; Get; Put;
        Shift 1; Add (int_of_char '#');
        Shift (-1); Loop ~~[
          Add (-1);
          Shift 3; Add 1;
          Shift (-2); Loop ~~[
            Loop ~~[
              Add (-1);
              Shift 1; Add 1;
              Shift (-1)
            ];
            Shift 2; Add (-1);
            Shift (-1); Add (-1);
            Shift (-1);
          ];
          Shift 2; Loop ~~[
            Add (-1);
            Shift (-3); Loop ~~[ Add (-1) ];
            Shift (-1); Add 1;
            Shift 4;
          ];
          Shift (-1); Loop ~~[
            Add (-1);
            Shift (-1); Add 1;
            Shift 1;
          ];
          Shift (-2);
        ];
        Shift 1; Loop ~~[
          Loop ~~[ Add (-1) ];
          Shift (-2); Add 1;
          Shift 2;
        ];
        Shift (-2);
      ]
    ];
    input = "How are you?\nI'm fine, thank you.# hoge";
    output = "How are you?\nI'm fine, thank you.#";
    is_ok = true;
    ptr_max = 4;
  };
  {
    name = "overflow";
    program = ~~[ Add (-1) ];
    input = "";
    output = "";
    is_ok = false;
    ptr_max = 0;
  };
  {
    name = "end of input";
    program = ~~[ Add 1; Loop ~~[ Get; Put ] ];
    input = "abc";
    output = "abc";
    is_ok = false;
    ptr_max = 0;
  };
  {
    name = "pointer out of range";
    program = ~~[ Shift (-1); Add 1 ];
    input = "";
    output = "";
    is_ok = false;
    ptr_max = 0;
  };
  {
    name = "skipped move_loop";
    program = ~~[ Loop ~~[ Add (-1); Shift (-1); Add 1; Shift 1; ] ];
    input = "";
    output = "";
    is_ok = true;
    ptr_max = 0;
  };
  {
    name = "ptr_max move_loop";
    program = ~~[ Add 1; Loop ~~[ Shift 1; Add 1; Shift (-1); Add (-1); ] ];
    input = "";
    output = "";
    is_ok = true;
    ptr_max = 1;
  }
]

let test = "bf" >::: List.map test_run cases

let () = run_test_tt_main test