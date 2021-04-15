open Batteries
open OUnit2
open Lib
open Reusable
open TestLib

let reusable_to_bf_code code =
  let program = Lexing.from_string code |> Parser.program Lexer.main in
  let dfn, code = Codegen.codegen program in
  let layout = Named.Layout.of_dfn dfn in
  Named.codegen layout code

module BfI = struct
  let code = ReusableCases.load_code "../../demo/bfi.bfr"

  let bf_code = reusable_to_bf_code code
end

let test_run ReusableCases.{ name; code; io_list; } =
  name >:: (fun _ ->
    let bf_code = reusable_to_bf_code code |> Bf.Cmd.list_to_string in
    io_list |> List.iter (fun (ipt, opt) ->
      let ipt = bf_code ^ "\\" ^ ipt in
      let state = Bf.run BfI.bf_code (String.enum ipt) in
      let Bf.State.{ err; _ } = state in
      assert_equal ~printer:(fun s -> s) opt (Bf.State.output_to_string state);
      assert_bool "error" (err = None)
    )
  )

let test = "bfi" >::: List.map test_run ReusableCases.cases

let () = run_test_tt_main test