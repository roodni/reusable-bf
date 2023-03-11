open Printf

let legacy = [
  "rev3.bfr";
  "nested_array.bfr";
  "switch.bfr";
  "fizzbuzz.bfr";
  "queens.bfr";
  "bfi.bfr";
]

let current = [
  ("addsubmul", "./sample/misc/bf/addsubmul.bfr");
]

let () =
  let opt_level =
    if Array.length Sys.argv = 2 then int_of_string Sys.argv.(1)
    else Ir.Opt.max_level
  in
  let programs =
    List.map
      (fun f -> (f, Filename.concat "./sample/misc/measurelen/" f))
      legacy
    @ current
  in
  List.iter
    (fun (name, path) ->
      let bf_code =
        try
          Reusable.Program.gen_bf_from_source ~opt_level path
        with
        | Reusable.Error.Exn_at msg_wi ->
            Reusable.Error.print msg_wi;
            exit 1;
      in
      printf "%s:\t%d\n" name (Bf.Code.length bf_code)
    )
    programs