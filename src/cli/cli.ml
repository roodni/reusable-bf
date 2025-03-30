module FileSystem : Metalang.Program.FileSystem = struct
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

let default_lib_dirs (getenv_opt: string -> string option) =
  let stdlib_dir =
    match
      getenv_opt "DUNE_SOURCEROOT",
      getenv_opt "OPAM_SWITCH_PREFIX"
    with
    | Some dune, _ ->
        Filename.concat dune "examples/lib" |> Option.some
    | None, Some opam ->
        Filename.concat opam "share/reusable-bf/lib" |> Option.some
    | None, None -> None
  in
  let paths =
    getenv_opt "BFML_LIB_PATH"
      |> Option.map (String.split_on_char ':')
      |> Option.value ~default:[]
      |> List.filter ((<>) "")
  in
  match stdlib_dir with
  | Some d -> paths @ [d]
  | None -> paths
