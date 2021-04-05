open Batteries
open OUnit2
open Lib
open Reusable

type case = {
  name: string;
  code: string;
  io_list: (string * string) list
}

let test_run { name; code; io_list; } =
  name >:: (fun _ ->
    let program = Lexing.from_string code |> Parser.program Lexer.main in
    let dfn, cmd_list = Program.codegen program in
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

let tests =
  test_run {
    name = "rev";
    io_list = [ ("hello\n", "olleh") ];
    code = "\
{
  buf: list_unlimited {
    c: cell;
    p: ptr;
  };
}

+ buf:c
! buf@p:c [
  > buf@p
  , buf@p:c
  - buf@p:c '\\n'
]
- buf:c
< buf@p
! buf@p:c [
  + buf@p:c '\\n'
  . buf@p:c
  < buf@p
]";
  }