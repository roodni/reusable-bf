open Printf

module Error = struct
  type info =
    | Loc of { file : string; line : int; col : int }
    | Unknown

  let create_info file line col =
    Loc { file; line; col; }

  let unknown_info = Unknown

  let info_of_position Lexing.{ pos_fname; pos_lnum; pos_cnum; pos_bol } =
    create_info pos_fname pos_lnum (1 + pos_cnum - pos_bol)

  let output_info channel = function
    | Loc { file; line; col; } ->
        fprintf channel "%s: line %d, col %d: " file line col
    | Unknown ->
        output_string channel "unknown location: "

  let error_at info msg =
    output_info stderr info;
    output_string stderr msg;
    output_string stderr "\n";
    exit 1
end