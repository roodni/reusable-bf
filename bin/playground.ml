open Js_of_ocaml
open Printf

type message_req = <
  files: <
    name: Js.js_string Js.t Js.prop;
    content: Js.js_string Js.t Js.prop;
  > Js.t Js.js_array Js.t Js.prop;

  optimize: int Js.prop;
  showLayout: bool Js.t Js.prop;
  entrypoint: Js.js_string Js.t Js.prop;
> Js.t

type message_res = <
  success: bool Js.t Js.prop;
  out: Js.js_string Js.t Js.prop;
  err: Js.js_string Js.t Js.prop;
> Js.t

exception Failed of string

let handler (req: message_req) =
  let files = req##.files |> Js.to_array in
  files |> Array.iter (fun file ->
      let name = file##.name |> Js.to_string in
      let content = file##.content |> Js.to_string in
      Sys_js.create_file ~name ~content
    );
  let optimize = req##.optimize in
  let show_layout = req##.showLayout |> Js.to_bool in

  print_endline (string_of_bool show_layout);
  printf "opt %d\n" optimize;

  let entrypoint = req##.entrypoint |> Js.to_string in

  let dirname = Filename.dirname entrypoint in
  let res: message_res =
    try
      let ir =
        try
          let program = Reusable.Program.load_from_source entrypoint in
          Reusable.Program.gen_ir ~path_limit:NoLimit dirname program
        with
        | Reusable.Error.Exn_at e -> begin
            let buf_err = Buffer.create 100 in
            let ppf_err = Format.formatter_of_buffer buf_err in
            Reusable.Error.print ~ppf:ppf_err e;
            Format.pp_print_flush ppf_err ();
            raise @@ Failed (Buffer.contents buf_err)
          end
      in
      
      let ir_field, ir_code = ir in
      let opt_context = Ir.Opt.{
          field = ir_field;
          code = ir_code;
          chan = stdout;
          dump = false;
        }
      in
      let layout, bf_code = Ir.Opt.codegen_by_level optimize opt_context in

      let len = Bf.Code.length bf_code in
      if len > 100000 then
        raise @@ Failed (Printf.sprintf "The output code size is too large (%d)" len);

      let out = Bf.Code.to_string bf_code in
      let err =
        if show_layout then begin
          let buf_err = Buffer.create 100 in
          let ppf_err = Format.formatter_of_buffer buf_err in
          Ir.Layout.output ppf_err layout;
          Format.pp_print_flush ppf_err ();
          Buffer.contents buf_err
        end else ""
      in
      object%js
        val mutable success = Js.bool true
        val mutable out = Js.string out
        val mutable err = Js.string err
      end
    with
    | Failed e ->
        object%js
          val mutable success = Js.bool false
          val mutable out = Js.string ""
          val mutable err = Js.string e
        end
  in
  Worker.post_message res
;;

let () = Worker.set_onmessage handler