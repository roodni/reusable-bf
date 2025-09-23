open OUnit2
open Printf
module Testcase = Helper.Testcase

let test_run Testcase.{ path; io_list; cell_type; _ } =
  path >:: (fun _ ->
    let bf_code = Helper.gen_bf_from_source path in
    io_list |> List.iter (fun (ipt, opt) ->
      let res, dump, opt_act =
        Bf.Exe.run_string
          ~input:ipt
          ~cell_type
          (Bf.Exe.from_code bf_code)
      in
      let failed = ref false in
      begin
        match res with
        | Ok () -> ()
        | Error msg ->
            print_newline ();
            print_endline "--- error ---";
            print_endline msg;
            failed := true
      end;
      if opt <> opt_act then begin
        print_newline ();
        print_endline "--- expected output ---";
        print_endline opt;
        print_endline "--- but got ---";
        print_endline opt_act;
        failed := true;
      end;
      if !failed then begin
        print_newline ();
        print_endline @@ Bf.Code.to_string bf_code;
        Bf.Exe.Dump.dump dump;
        printf "^^^ %s ^^^\n" path;
        flush_all ();
        assert_bool "fail" false
      end
    )
  )



let test = "reusable" >::: List.map test_run Testcase.cases

let () = run_test_tt_main test