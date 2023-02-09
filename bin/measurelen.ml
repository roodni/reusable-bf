open Printf

let programs = [
  ("rev3", "rev3.bfr");
  ("nested", "nested_array.bfr");
  ("switch", "switch.bfr");
  ("fizzbuzz", "fizzbuzz.bfr");
  ("queens", "queens.bfr");
  ("bfi", "bfi.bfr");
]

let () =
  List.iter
    (fun (name, path) ->
      let bf_code =
        try
          Reusable.Program.gen_bf_from_source
            (Filename.concat "./sample/misc/measurelen/" path)
        with
        | Reusable.Error.Exn_at msg_wi ->
            Reusable.Error.print msg_wi;
            exit 1;
      in
      let buf = Bf.Code.to_buffer bf_code in
      printf "%s:\t%d\n" name (Buffer.length buf)
    )
    programs