type case = {
  name: string;
  io_list: (string * string) list;
  filename: string
}

let cases =
  [ { name = "rev";
      io_list = [ ("hello\n", "olleh") ];
      filename = "sample/misc/rev.bfr";
    };
    { name = "rev2";
      io_list = [ ("hello\na", "olleh") ];
      filename = "sample/misc/rev2.bfr";
    };
    { name = "echo";
      io_list = [ ("Hello, world!#test", "Hello, world!#") ];
      filename = "sample/misc/echo.bfr"
    };
    { name = "hygienic";
      io_list = [ ("", "O") ];
      filename = "sample/misc/hygienic.bfr";
    };
    { name = "rev3";
      io_list = [ ("hello\na", "olleh") ];
      filename = "sample/misc/rev3.bfr";
    };
    { name = "prime";
      io_list = [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
      filename = "sample/misc/prime.bfr";
    };
    { name = "switch_nat";
      io_list = [ ("0", "B"); ("1", "F"); ("2", "R"); ("3", "3"); ("7", "7") ];
      filename = "sample/misc/switch_nat.bfr";
    };
    { name = "hello";
      io_list = [ ("", "Hello World!\n") ];
      filename = "sample/hello.bfr";
    };
    { name = "sort";
      io_list = [ ("", "34567") ];
      filename = "sample/misc/sort.bfr"
    };
    { name = "switch";
      io_list = [
        ("+", "INC"); ("-", "DEC");
        (".", "PUT"); (",", "GET");
        ("[", "WHILE"); ("]", "WEND");
        (">", "SHR"); ("<", "SHL");
        ("a", "OTHER"); ("\n", "OTHER");
      ];
      filename = "sample/misc/switch.bfr"
    };
    { name = "import";
      io_list = [ ("", "BC123") ];
      filename = "sample/misc/import_a.bfr"
    };
    { name = "nested_array";
      io_list = [ ("", "XYPQqRSrs012MNmn") ];
      filename = "sample/misc/nested_array.bfr"
    };
    { name = "queens";
      io_list =
        [ ("2\n", "NO ANSWER\n");
          ( "4\n",
            ". Q . . \n" ^
            ". . . Q \n" ^
            "Q . . . \n" ^
            ". . Q . \n" )
        ];
      filename = "sample/misc/queens.bfr"
    };
    { name = "bug: similar indexed selectors";
      io_list = [ ("", "01") ];
      filename = "sample/misc/bug_similar_indexed_selectors.bfr"
    };
    { name = "fizzbuzz";
      io_list = [ ("16\n", "1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 ") ];
      filename = "sample/misc/fizzbuzz.bfr"
    };
    { name = "bug: diving cell reset fail";
      io_list = [ ("", "!!") ];
      filename = "sample/misc/bug_diving_cell_reset_fail.bfr";
    };
  ]
|> List.map
  (fun case -> { case with filename="../../" ^ case.filename })