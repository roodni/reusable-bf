open Printf
open Support.Info

let programs = [
  (* ("error", "./sample/misc/error/eval_nd-var_var.bfr"); *)
  ("rev3", "./sample/misc/rev3.bfr");
  ("nested", "./sample/misc/nested_array.bfr");
  ("switch", "./sample/misc/switch.bfr");
  ("fizzbuzz", "./sample/misc/fizzbuzz.bfr");
  ("queens", "./sample/misc/queens.bfr");
  ("bfi", "./sample/bfi.bfr");
]

let () =
  List.iter
    (fun (name, path) ->
      let bf_code =
        try Reusable.Program.gen_bf_from_source path with
        | Reusable.Error.Exn_at msg_wi ->
            Reusable.Error.print msg_wi;
            exit 1;
      in
      let buf = Bf.Code.to_buffer bf_code in
      printf "%s:\t%d\n" name (Buffer.length buf)
    )
    programs