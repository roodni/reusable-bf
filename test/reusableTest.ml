open Batteries
open OUnit2
open Lib
open Reusable

type case = {
  name: string;
  io_list: (string * string) list;
  code: string;
}

let load_code path =
	let input = open_in path in
	let code = BatIO.read_all input in
	close_in input;
	code

let test_run { name; code; io_list; } =
  name >:: (fun _ ->
    let program = Lexing.from_string code |> Parser.program Lexer.main in
    let dfn, cmd_list = Codegen.codegen program in
    let layout = Named.Layout.of_dfn dfn in
    let bf_code = Named.codegen layout cmd_list in
    io_list |> List.iter (fun (ipt, opt) ->
      let state = Bf.run bf_code (String.enum ipt) in
      let Bf.State.{ err; _ } = state in
      if err <> None || opt <> Bf.State.output_to_string state then begin
        print_endline @@ Bf.Cmd.list_to_string bf_code;
        print_endline "--- expected output ---";
        print_endline opt;
        print_newline ();
        Bf.State.dump state;
        assert_bool "fail" false
      end
    )
  )

let cases = [
  {
    name = "rev";
    io_list = [ ("hello\n", "olleh") ];
		code = load_code "../demo/rev.bfr";
  };
  {
    name = "rev2";
    io_list = [ ("hello\na", "olleh") ];
    code = load_code "../demo/rev2.bfr";
  };
  {
    name = "echo";
    io_list = [ ("Hello, world!#test", "Hello, world!#") ];
    code = load_code "../demo/echo.bfr"
  };
  {
    name = "hygienic";
    io_list = [ ("", "O") ];
    code = load_code "../demo/hygienic.bfr";
  };
  {
    name = "rev3";
    io_list = [ ("hello\na", "olleh") ];
    code = load_code "../demo/rev3.bfr";
  };
  {
    name = "prime";
    io_list = [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
    code = load_code "../demo/prime.bfr";
  };
  {
    name = "switch_nat";
    io_list = [ ("0", "A"); ("1", "1B"); ("2", "C"); ("3", "3"); ("7", "7") ];
    code = load_code "../demo/switch_nat.bfr";
  };
  {
    name = "str";
    io_list = [ ("", "hello\nworld\n") ];
    code = load_code "../demo/str.bfr";
  };
  {
    name = "sort";
    io_list = [ ("", "34567") ];
    code = load_code "../demo/sort.bfr"
  };
  {
    name = "switch";
    io_list = [
      ("+", "INC"); ("-", "DEC");
      (".", "PUT"); (",", "GET");
      ("[", "WHILE"); ("]", "WEND");
      (">", "SHR"); ("<", "SHL");
      ("a", "OTHER"); ("\n", "OTHER");
    ];
    code = load_code "../demo/switch.bfr"
  }
]

let tests = "reusable" >::: List.map test_run cases