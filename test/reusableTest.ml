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

let _ = print_string @@ Sys.getcwd ()

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
    name = "rev3";
    io_list = [ ("hello\na", "olleh") ];
    code = load_code "../demo/rev3.bfr";
  }
]

let tests = "reusable" >::: List.map test_run cases