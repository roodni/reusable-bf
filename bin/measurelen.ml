open Printf

let programs = [
  ("rev3", "./test/reusable/code/rev3.bfr");
  ("nested", "./test/reusable/code/nested_array.bfr");
  ("fizzbuzz", "./test/reusable/code/fizzbuzz.bfr");
  ("queens", "./test/reusable/code/queens.bfr");
  ("bfi", "./sample/bfi.bfr");
]

let () =
  List.iter
    (fun (name, path) ->
      let buf =
        Reusable.IrGen.gen_bf_from_source path
        |> Bf.Code.to_buffer
      in
      printf "%s:\t%d\n" name (Buffer.length buf)
    )
    programs