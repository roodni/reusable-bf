open Batteries
open OUnit2
open Lib
open Reusable

type case = {
  name: string;
  io_list: (string * string) list;
  code: string;
}

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
  };
  {
    name = "rev2";
    io_list = [ ("hello\na", "olleh") ];
    code = "\
{
	buf: list_unlimited {
		c: cell;
		p: ptr;
	};
}

$var {	# comment
	f: cell;
} in

$let p = buf@p in
$let c = p:c in

+ f
! f [
	, c
	- c '\\n'
	? c [] [
		- f
	]
	+ c '\\n'
	> p
]

< p
! p [
	< p
	. c
]";
  };
  {
    name = "echo";
    io_list = [ ("Hello, world!#test", "Hello, world!#") ];
    code = "\
{}

$let del = fun x -> [
	! x [ - x ]
] in

$let if_eq = fun a -> fun b -> fun then -> [
	! a [
		- a
		? b [ - b ] [
			*del a
			*then
		]
	]
	! b [
		*del b
		*then
	]
] in

$var {
	cont: cell;
} in
+ cont
! cont [
	- cont
	$var {
		input: cell;
		diff: cell;
	} in
	, input
	. input
	+ diff '#'
	*if_eq input diff [ + cont ]
]"
  };
  {
    name = "rev3";
    io_list = [ ("hello\na", "olleh") ];
    code = "\
{
	buf: list_unlimited {
		c: cell;
		p: ptr;
	};
}

$let input_end = '\\n' in
$let p = buf@p in
$let c = p:c in

$dive p [
	$var {
		f: cell;
	} in
	+ f
	! f [
		, c
		- c input_end
		? c [] [ - f ]
		+ c input_end
		> p
	]
]

< p
! p [
	< p
	. c
]";
  }
]

let tests = "reusable" >::: List.map test_run cases