open OUnit2


module BfI = struct
  let filename = Filename.concat Testcase.source_root "examples/bfi.bfml"
  let bf_exe =
    Reusable.Program.gen_bf_from_source filename
    |> Bf.Exe.from_code
end

let test_run Testcase.{ path; io_list; cell_type; _ } =
  path >:: (fun _ ->
    let bf_code =
      Reusable.Program.gen_bf_from_source path
      |> Bf.Code.to_string
    in
    io_list |> List.iter (fun (ipt, opt) ->
      let ipt = bf_code ^ "\\" ^ ipt in
      let res, _, opt_act =
        Bf.Exe.run_string
          ~cell_type
          ~input:ipt
          BfI.bf_exe
      in
      assert_equal ~printer:(fun s -> s) opt opt_act;
      assert_bool "error" (Result.is_ok res)
    )
  )

let test =
  "bfi" >::: List.map test_run
    Testcase.(cases |> List.filter (fun c -> c.run_bfi))

let () = run_test_tt_main test