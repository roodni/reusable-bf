type case = {
  name: string;
  io_list: (string * string) list;
  filename: string
}

let cases = [
  {
    name = "rev";
    io_list = [ ("hello\n", "olleh") ];
		filename = "code/rev.bfr";
  };
  {
    name = "rev2";
    io_list = [ ("hello\na", "olleh") ];
    filename = "code/rev2.bfr";
  };
  {
    name = "echo";
    io_list = [ ("Hello, world!#test", "Hello, world!#") ];
    filename = "code/echo.bfr"
  };
  {
    name = "hygienic";
    io_list = [ ("", "O") ];
    filename = "code/hygienic.bfr";
  };
  {
    name = "rev3";
    io_list = [ ("hello\na", "olleh") ];
    filename = "code/rev3.bfr";
  };
  {
    name = "prime";
    io_list = [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
    filename = "code/prime.bfr";
  };
  {
    name = "switch_nat";
    io_list = [ ("0", "B"); ("1", "F"); ("2", "R"); ("3", "3"); ("7", "7") ];
    filename = "code/switch_nat.bfr";
  };
  {
    name = "hello";
    io_list = [ ("", "hello world\n") ];
    filename = "../../sample/hello.bfr";
  };
  {
    name = "sort";
    io_list = [ ("", "34567") ];
    filename = "code/sort.bfr"
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
    filename = "code/switch.bfr"
  };
  {
    name = "import";
    io_list = [ ("", "BC123") ];
    filename = "code/import_a.bfr"
  };
  {
    name = "nested_array";
    io_list = [ ("", "XYPQqRSrs012MNmn") ];
    filename = "code/nested_array.bfr"
  };
  {
    name = "queens";
    io_list =
      [ ("2\n", "NO ANSWER\n");
        ( "4\n",
          ". Q . . \n" ^
          ". . . Q \n" ^
          "Q . . . \n" ^
          ". . Q . \n" )
      ];
    filename = "code/queens.bfr"
  };
  {
    name = "bugfix: indexed to indexed";
    io_list = [ ("", "01") ];
    filename = "code/indexed_to_indexed.bfr"
  }
]