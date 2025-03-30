open Js_of_ocaml

module Message = struct
  open Js
  
  type req = <
    files: <
      name: js_string t prop;
      content: js_string t prop;
    > t js_array t prop;

    optimize: int prop;
    showLayout: bool t prop;
    maxLength: int prop;
    entrypoint: js_string t prop;
  > t

  type res = <
    success: bool t readonly_prop;
    out: js_string t readonly_prop;
    err: js_string t readonly_prop;
  > t
end

exception Failed of string

module Js = struct
  include Js

  let to_string o =
    if Js.typeof o |> Js.to_string = "string"
      then Js.to_string o
      else assert false
  
  let to_bool o =
    if Js.typeof o |> Js.to_string = "boolean"
      then Js.to_bool o
      else assert false

  let to_number o =
    if Obj.magic o |> Js.typeof |> Js.to_string = "number"
      then o
      else assert false
end

module FileSystem = struct
  (* TODO: あとでPlayground用にする *)
  module FileId = struct
    type t = string
    let compare = String.compare
  end

  let file_exists path = Sys.file_exists path

  let path_to_file_id path = Unix.realpath path

  let open_file path f =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let lexbuf = Lexing.from_channel ic in
        f lexbuf
      )
end

module Program = Metalang.Program.Make(FileSystem)

let handler (req: Message.req) =
  let files = req##.files |> Js.to_array in
  files |> Array.iter (fun file ->
      let name = file##.name |> Js.to_string in
      let content = file##.content |> Js.to_string in
      Sys_js.create_file ~name ~content
    );
  let optimize = req##.optimize |> Js.to_number in
  let max_length = req##.maxLength  |> Js.to_number in
  let show_layout = req##.showLayout |> Js.to_bool in

  let entrypoint = req##.entrypoint |> Js.to_string in

  let base_dir = Filename.dirname entrypoint in
  let res: Message.res =
    try
      let ir =
        try
          let program = Program.load_by_path entrypoint in
          Program.gen_ir ~lib_dirs:["/"] ~base_dir program
        with
        | Metalang.Error.Exn_at e -> begin
            let buf_err = Buffer.create 100 in
            let ppf_err = Format.formatter_of_buffer buf_err in
            Metalang.Error.print ~ppf:ppf_err e;
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
      if len > max_length then
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
        val success = Js.bool true
        val out = Js.string out
        val err = Js.string err
      end
    with
    | Failed e ->
        object%js
          val success = Js.bool false
          val out = Js.string ""
          val err = Js.string e
        end
  in
  Worker.post_message res
;;

let () = Worker.set_onmessage handler