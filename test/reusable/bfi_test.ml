open OUnit2


module BfI = struct
  let filename = "../../sample/bfi.bfr"
  let bf_exe =
    Reusable.IrGen.gen_bf_from_source filename
    |> Bf.Exe.from_code
end

let test_run Testcase.{ name; filename; io_list; } =
  name >:: (fun _ ->
    let bf_code =
      Reusable.IrGen.gen_bf_from_source filename
      |> Bf.Code.to_string
    in
    io_list |> List.iter (fun (ipt, opt) ->
      let ipt = bf_code ^ "\\" ^ ipt in
      let res, _, opt_act =
        Bf.Exe.run_string
          ~cell_type:Bf.Exe.WrapAround256
          ~input:(Stream.of_string ipt)
          BfI.bf_exe
      in
      assert_equal ~printer:(fun s -> s) opt opt_act;
      assert_bool "error" (Result.is_ok res)
    )
  )

let test = "bfi" >::: List.map test_run Testcase.cases

let () = run_test_tt_main test