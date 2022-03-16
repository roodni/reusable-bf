open OUnit2
open Lib

let reusable_to_bf_code filename =
  let dirname = Filename.dirname filename in
  let program = Reusable.load_program filename in
  let dfn, code = Reusable.codegen_all dirname program in
  let layout = Named.Layout.of_dfn dfn in
  Named.codegen layout code

module BfI = struct
  let filename = "../../sample/bfi.bfr"
  let bf_exe = reusable_to_bf_code filename |> Bf.Exe.from_code
end

let test_run Testcase.{ name; filename; io_list; } =
  name >:: (fun _ ->
    let bf_code = reusable_to_bf_code filename |> Bf.Code.to_string in
    io_list |> List.iter (fun (ipt, opt) ->
      let ipt = bf_code ^ "\\" ^ ipt in
      let res, _, opt_act =
        Bf.Exe.run_string
          ~cell_type:Bf.Overflow256
          ~input:(Stream.of_string ipt)
          BfI.bf_exe
      in
      assert_equal ~printer:(fun s -> s) opt opt_act;
      assert_bool "error" (Result.is_ok res)
    )
  )

let test = "bfi" >::: List.map test_run Testcase.cases

let () = run_test_tt_main test