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
