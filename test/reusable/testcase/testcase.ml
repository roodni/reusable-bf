type case = {
  path: string;
  io_list: (string * string) list;
  run_bfi: bool;
  cell_type: Bf.Exe.cell_type
}

let source_root = Sys.getenv "DUNE_SOURCEROOT"

let case ?(run_bfi=true) ?(cell_type=Bf.Exe.Overflow256) path io_list =
  { path = Filename.concat source_root path;
    io_list;
    run_bfi; cell_type;
  }

let cases =
  [
    case "sample/misc/bf/rev.bfr"
      [ ("hello\n", "olleh") ];
    case "sample/misc/bf/rev2.bfr"
      [ ("hello\na", "olleh") ];
    case "sample/misc/bf/echo.bfr"
      [ ("Hello, world!#test", "Hello, world!#") ];
    case "sample/misc/metalang/hygienic.bfr"
      [ ("", "O") ];
    case "sample/misc/bf/rev3.bfr"
      [ ("hello\na", "olleh") ];
    case "sample/misc/metalang/prime.bfr"
      [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
    case "sample/misc/bf/switch_nat.bfr"
      [ ("0", "B"); ("1", "F"); ("2", "R"); ("3", "3"); ("7", "7") ];
    case "sample/hello.bfr"
      [ ("", "Hello World!\n") ];
    case "sample/misc/metalang/sort.bfr"
      [ ("", "34567") ];
    case "sample/misc/bf/switch.bfr"
      [ ("+", "INC"); ("-", "DEC");
        (".", "PUT"); (",", "GET");
        ("[", "WHILE"); ("]", "WEND");
        (">", "SHR"); ("<", "SHL");
        ("a", "OTHER"); ("\n", "OTHER");
      ];
    case "sample/misc/metalang/import_a.bfr"
      [ ("", "BC123") ];
    case "sample/misc/bf/nested_array.bfr"
      [ ("", "XYPQqRSrs012MNmn") ];
    case "sample/misc/bf/queens.bfr" ~cell_type:WrapAround256
      [ ("2\n", "NO ANSWER\n");
        ( "4\n",
          ". Q . . \n" ^
          ". . . Q \n" ^
          "Q . . . \n" ^
          ". . Q . \n" )
      ];
    case "sample/misc/bug/similar_indexed_selectors.bfr"
      [ ("", "01") ];
    case "sample/fizzbuzz.bfr"
      [ ("16\n", "1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 ") ];
    case "sample/misc/bug/diving_cell_reset_fail.bfr"
      [ ("", "!!") ];
    case "sample/misc/metalang/bop.bfr"
      [ ("", "O") ];
    case "sample/misc/metalang/submodule.bfr"
      [ ("", "ABCC") ];
    case "sample/misc/bug/const_analyze_loop.bfr"
      [ ("A", "A"); ("B", "") ];
    case "sample/lib/counter.bfr"
      [ ("A", "65"); ("0", "48"); ("{", "123") ];
    case "sample/misc/bf/counter_test.bfr"
      [ ("", "0\n123\n0\n4") ];
    case "sample/lib/fixedint.bfr"  ~run_bfi:false
      [ ("0 0\n", "0 0");
        ("12 34\n", "46 408");
        ("-1 2\n", "1 -2");
        ("3 -4\n", "-1 -12");
        ("-5 -6\n", "-11 30");
      ];
    case "sample/misc/bf/addsubmul.bfr" ~run_bfi:false
      [ ("3 1\n", "4\n");
        ("4 -2\n", "6\n");
        ("0 0\n", "0\n");
        ("1000 -1000\n", "2000\n") ];
    case "sample/misc/bug/const_analyze_loop_heavy.bfr"
      [ ("\n", "") ];
    case "sample/misc/bug/shift_liveness.bfr"
      [ ("", "A") ];
  ]