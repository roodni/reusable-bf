open Printf

let programs = [
  "./sample/misc/bf/rev3.bfr";
  "./sample/misc/bf/nested_array.bfr";
  "./sample/misc/bf/switch.bfr";
  "./sample/fizzbuzz.bfr";
  "./sample/misc/bf/queens.bfr";
  "./sample/bfi.bfr";
  "./sample/addsubmul.bfr";
  "./sample/misc/bf/ifelse.bfr";
]

let () =
  let opt_level =
    if Array.length Sys.argv = 2 then int_of_string Sys.argv.(1)
    else Ir.Opt.max_level
  in
  List.iter
    (fun path ->
      let name = Filename.basename path |> Filename.chop_extension in
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