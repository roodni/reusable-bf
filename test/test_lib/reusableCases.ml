open Batteries

let project_root =
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

let filename_from_current filename_from_root =
	Filename.concat project_root filename_from_root

type case = {
  name: string;
  io_list: (string * string) list;
  filename: string
}

let cases = [
  {
    name = "rev";
    io_list = [ ("hello\n", "olleh") ];
		filename = filename_from_current "sample/rev.bfr";
  };
  {
    name = "rev2";
    io_list = [ ("hello\na", "olleh") ];
    filename = filename_from_current "sample/rev2.bfr";
  };
  {
    name = "echo";
    io_list = [ ("Hello, world!#test", "Hello, world!#") ];
    filename = filename_from_current "sample/echo.bfr"
  };
  {
    name = "hygienic";
    io_list = [ ("", "O") ];
    filename = filename_from_current "sample/hygienic.bfr";
  };
  {
    name = "rev3";
    io_list = [ ("hello\na", "olleh") ];
    filename = filename_from_current "sample/rev3.bfr";
  };
  {
    name = "prime";
    io_list = [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
    filename = filename_from_current "sample/prime.bfr";
  };
  {
    name = "switch_nat";
    io_list = [ ("0", "B"); ("1", "F"); ("2", "R"); ("3", "3"); ("7", "7") ];
    filename = filename_from_current "sample/switch_nat.bfr";
  };
  {
    name = "str";
    io_list = [ ("", "hello world\n") ];
    filename = filename_from_current "sample/str.bfr";
  };
  {
    name = "sort";
    io_list = [ ("", "34567") ];
    filename = filename_from_current "sample/sort.bfr"
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
    filename = filename_from_current "sample/switch.bfr"
  };
  {
    name = "import";
    io_list = [ ("", "BC123") ];
    filename = filename_from_current "sample/import_a.bfr"
  }
]