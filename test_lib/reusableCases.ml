open Batteries

let find_project_root () =
  let cwd_list = Sys.getcwd () |>
    String.split_on_string ~by:(Filename.dir_sep) |>
    List.rev
  in
  let dir_list_to_string dir_list =
    List.rev dir_list |> String.concat (Filename.dir_sep)
  in
  let rec find dir_list =
    if Sys.file_exists @@ dir_list_to_string ("dune-project" :: dir_list) then
      dir_list
    else find (List.tl dir_list)
  in
  find cwd_list |> dir_list_to_string

let load_code path () =
  let project_root = find_project_root () in
	let input = open_in (project_root ^ "/" ^ path) in
	let code = BatIO.read_all input in
	close_in input;
	code

type case = {
  name: string;
  io_list: (string * string) list;
  code: unit -> string;
}

let cases = [
  {
    name = "rev";
    io_list = [ ("hello\n", "olleh") ];
		code = load_code "demo/rev.bfr";
  };
  {
    name = "rev2";
    io_list = [ ("hello\na", "olleh") ];
    code = load_code "demo/rev2.bfr";
  };
  {
    name = "echo";
    io_list = [ ("Hello, world!#test", "Hello, world!#") ];
    code = load_code "demo/echo.bfr"
  };
  {
    name = "hygienic";
    io_list = [ ("", "O") ];
    code = load_code "demo/hygienic.bfr";
  };
  {
    name = "rev3";
    io_list = [ ("hello\na", "olleh") ];
    code = load_code "demo/rev3.bfr";
  };
  {
    name = "prime";
    io_list = [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
    code = load_code "demo/prime.bfr";
  };
  {
    name = "switch_nat";
    io_list = [ ("0", "A"); ("1", "1B"); ("2", "C"); ("3", "3"); ("7", "7") ];
    code = load_code "demo/switch_nat.bfr";
  };
  {
    name = "str";
    io_list = [ ("", "hello\nworld\n") ];
    code = load_code "demo/str.bfr";
  };
  {
    name = "sort";
    io_list = [ ("", "34567") ];
    code = load_code "demo/sort.bfr"
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
    code = load_code "demo/switch.bfr"
  }
]