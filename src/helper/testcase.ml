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
    case "examples/misc/bf/rev.bfml"
      [ ("hello\n", "olleh") ];
    case "examples/misc/bf/rev2.bfml"
      [ ("hello\na", "olleh") ];
    case "examples/misc/bf/echo.bfml"
      [ ("Hello World!#test", "Hello World!#") ];
    case "examples/misc/metalang/hygienic.bfml"
      [ ("", "O") ];
    case "examples/misc/bf/rev3.bfml"
      [ ("hello\na", "olleh") ];
    case "examples/misc/metalang/prime.bfml"
      [ ("", "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 ")];
    case "examples/misc/bf/switch_nat.bfml"
      [ ("0", "B"); ("1", "F"); ("2", "R"); ("3", "3"); ("7", "7") ];
    case "examples/hello.bfml"
      [ ("", "Hello World!\n") ];
    case "examples/hello2.bfml"
      [ ("", "Hello World!\n") ];
    case "examples/misc/metalang/sort.bfml"
      [ ("", "34567") ];
    case "examples/misc/bf/switch.bfml"
      [ ("+", "INC"); ("-", "DEC");
        (".", "PUT"); (",", "GET");
        ("[", "WHILE"); ("]", "WEND");
        (">", "SHR"); ("<", "SHL");
        ("a", "OTHER"); ("\n", "OTHER");
      ];
    case "examples/misc/metalang/import_a.bfml"
      [ ("", "BC123") ];
    case "examples/misc/bf/nested_array.bfml"
      [ ("", "XYPQqRSrs012MNmn") ];
    case "examples/misc/bf/queens.bfml" ~cell_type:WrapAround256
      [ ("2\n", "NO ANSWER\n");
        ( "4\n",
          ". Q . . \n" ^
          ". . . Q \n" ^
          "Q . . . \n" ^
          ". . Q . \n" )
      ];
    case "examples/misc/bug/similar_indexed_selectors.bfml"
      [ ("", "01") ];
    case "examples/fizzbuzz.bfml"
      [ ("16\n", "1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 ") ];
    case "examples/misc/bug/diving_cell_reset_fail.bfml"
      [ ("", "!!") ];
    case "examples/misc/metalang/bop.bfml"
      [ ("", "O") ];
    case "examples/misc/metalang/submodule.bfml"
      [ ("", "ABCC") ];
    case "examples/misc/bug/const_analyze_loop.bfml"
      [ ("A", "A"); ("B", "") ];
    case "examples/lib/std/counter.bfml"
      [ ("A", "65"); ("0", "48"); ("{", "123") ];
    case "examples/misc/libtest/counter/counter_test.bfml"
      [ ("", "0\n123\n0\n4") ];
    case "examples/misc/libtest/fixedint/fixedint_addmul.bfml" ~run_bfi:false
      [ ("0 0\n", "0 0");
        ("12 34\n", "46 408");
        ("-1 2\n", "1 -2");
        ("3 -4\n", "-1 -12");
        ("-5 -6\n", "-11 30");
      ];
    case "examples/misc/libtest/fixedint/abc087a.bfml" ~run_bfi:false
      [ ("1234\n150\n100\n", "84");
        ("1000\n108\n108\n", "28");
        ("579\n123\n456\n", "0");
        ("7477\n549\n593\n", "405");
      ];
    case "examples/addsubmul.bfml" ~run_bfi:false
      [ ("3 1\n", "4\n");
        ("4 -2\n", "6\n");
        ("0 0\n", "0\n");
        ("1000 -1000\n", "2000\n") ];
    case "examples/misc/bug/const_analyze_loop_heavy.bfml"
      [ ("\n", "") ];
    case "examples/misc/bug/shift_liveness.bfml"
      [ ("", "A") ];
    case "examples/misc/metalang/patlist.bfml"
      [ ("", "123456") ];
    case "examples/misc/metalang/pipe.bfml" ~run_bfi:false
      [ ("", "OK") ];
    case "examples/misc/metalang/private.bfml" ~run_bfi:false
      [ ("", "OK") ];
    case "examples/misc/metalang/include.bfml" ~run_bfi:false
      [ ("", "OK") ];
    case "examples/misc/metalang/semicolon.bfml" ~run_bfi:false
      [ ("", "OK") ];
    case "examples/misc/metalang/ifunit.bfml" ~run_bfi:false
      [ ("", "OK") ];
    case "examples/misc/metalang/tuple.bfml"
      [ ("", "NONASSOC") ];
  ]