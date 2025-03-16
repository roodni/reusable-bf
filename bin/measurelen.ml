open Printf

let programs = [
  "./examples/misc/bf/rev3.bfml";
  "./examples/misc/bf/nested_array.bfml";
  "./examples/misc/bf/switch.bfml";
  "./examples/fizzbuzz.bfml";
  "./examples/misc/bf/queens.bfml";
  "./examples/bfi.bfml";
  "./examples/addsubmul.bfml";
  "./examples/misc/bf/ifelse.bfml";
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
          Helper.gen_bf_from_source ~opt_level path
        with
        | Metalang.Error.Exn_at msg_wi ->
            Metalang.Error.print msg_wi;
            exit 1;
      in
      printf "%s:\t%d\n" name (Bf.Code.length bf_code)
    )
    programs