type case = {
  path: string;
  io_list: (string * string) list;
  run_bfi: bool;
}

let case ?(run_bfi=true) path io_list =
  { path = Filename.concat "../../" path;
    io_list;
    run_bfi }

let cases =
  [
    case "sample/misc/rev.bfr"
      [ ("hello\n", "olleh") ];
    case "sample/misc/rev2.bfr"
      [ ("hello\na", "olleh") ];
    case "sample/misc/echo.bfr"
      [ ("Hello, world!#test", "Hello, world!#") ];
    case "sample/misc/hygienic.bfr"
      [ ("", "O") ];
    case "sample/misc/rev3.bfr"
      [ ("hello\na", "olleh") ];
    case "sample/misc/prime.bfr"
      [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
    case "sample/misc/switch_nat.bfr"
      [ ("0", "B"); ("1", "F"); ("2", "R"); ("3", "3"); ("7", "7") ];
    case "sample/hello.bfr"
      [ ("", "Hello World!\n") ];
    case "sample/misc/sort.bfr"
      [ ("", "34567") ];
    case "sample/misc/switch.bfr"
      [ ("+", "INC"); ("-", "DEC");
        (".", "PUT"); (",", "GET");
        ("[", "WHILE"); ("]", "WEND");
        (">", "SHR"); ("<", "SHL");
        ("a", "OTHER"); ("\n", "OTHER");
      ];
    case "sample/misc/import_a.bfr"
      [ ("", "BC123") ];
    case "sample/misc/nested_array.bfr"
      [ ("", "XYPQqRSrs012MNmn") ];
    case "sample/misc/queens.bfr"
      [ ("2\n", "NO ANSWER\n");
        ( "4\n",
          ". Q . . \n" ^
          ". . . Q \n" ^
          "Q . . . \n" ^
          ". . Q . \n" )
      ];
    case "sample/misc/bug_similar_indexed_selectors.bfr"
      [ ("", "01") ];
    case "sample/fizzbuzz.bfr"
      [ ("16\n", "1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 ") ];
    case "sample/misc/bug_diving_cell_reset_fail.bfr"
      [ ("", "!!") ];
    case "sample/misc/bop.bfr"
      [ ("", "O") ];
    case "sample/misc/submodule.bfr"
      [ ("", "ABCC") ];
    case "sample/misc/bug_const_analyze_loop.bfr"
      [ ("A", "A"); ("B", "") ];
    case "sample/lib/counter.bfr"
      [ ("", "0\n123\n0\n4") ];
    case "sample/lib/fixedint.bfr"  ~run_bfi:false
      [ ("0 0\n", "0 0");
        ("12 34\n", "46 408");
        ("-1 2\n", "1 -2");
        ("3 -4\n", "-1 -12");
        ("-5 -6\n", "-11 30");
      ];
    case "sample/misc/addsubmul.bfr" ~run_bfi:false
      [ ("3 1\n", "4\n");
        ("4 -2\n", "6\n");
        ("0 0\n", "0\n");
        ("1000 -1000\n", "2000\n") ];
  ]