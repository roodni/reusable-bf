open Lib
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
      let dirname = Filename.dirname path in
      let program = Reusable.load_program path in
      let field, code = Reusable.codegen_all dirname program in
      let layout = Named.Layout.from_field code field in
      let bf = Named.gen_bf layout code in
      let buf = Bf.Code.to_buffer bf in
      printf "%s:\t%d\n" name (Buffer.length buf)
    )
    programs