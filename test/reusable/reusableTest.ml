open Batteries
open OUnit2
open Lib
open TestLib

let test_run ReusableCases.{ name; filename; io_list; } =
  name >:: (fun _ ->
    let dirname = Filename.dirname filename in
    let program = Reusable.load_program filename in
    let dfn, cmd_list = Reusable.codegen_all dirname program in
    let layout = Named.Layout.of_dfn dfn in
    let bf_code = Named.codegen layout cmd_list in
    io_list |> List.iter (fun (ipt, opt) ->
      let res, tape, opt_act =
        Bf.Exe.run_string
          ~input:(Stream.of_string ipt)
          ~cell_type:Bf.Overflow256
          (Bf.Exe.from_code bf_code)
      in
      let failed = ref false in
      begin
        match res with
        | Ok () -> ()
        | Error msg ->
            print_endline "--- error ---";
            print_endline msg;
            failed := true
      end;
      if opt <> opt_act then begin
        print_endline @@ Bf.Code.to_string bf_code;
        print_endline "--- expected output ---";
        print_endline opt;
        print_endline "--- but got ---";
        print_endline opt_act;
        failed := true;
      end;
      if !failed then begin
        print_newline ();
        Bf.Exe.Tape.dump tape;
        assert_bool "fail" false
      end
    )
  )



let test = "reusable" >::: List.map test_run ReusableCases.cases

let () = run_test_tt_main test