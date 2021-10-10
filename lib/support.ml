open Printf

module Pervasive = struct
  let (--) a b =
    let len = b - a + 1 in
    if len < 0 then []
    else List.init len (fun i -> a + i)

  module String = struct
    include String
    let repeat s n =
      let buf = Buffer.create (String.length s * n) in
      (1 -- n) |> List.iter (fun _ -> Buffer.add_string buf s);
      Buffer.contents buf
  end
end

module Error = struct
  type info =
    | Loc of {
        fname : string;
        l_start : int;
        l_end : int;
        c_start : int;
        c_end : int;
      }
    | Unknown

  type 'a withinfo = { i : info; v : 'a }

  let unknown_info = Unknown

  let position_to_flc Lexing.{ pos_fname; pos_lnum; pos_cnum; pos_bol } =
    (pos_fname, pos_lnum, 1 + pos_cnum - pos_bol)

  let create_info pos1 pos2 =
    let (f, l1, c1) = position_to_flc pos1 in
    let (_, l2, c2) = position_to_flc pos2 in
    Loc {
      fname = f;
      l_start = l1; l_end = l2;
      c_start = c1; c_end = c2 }

  let withinfo i v = { i; v; }

  let merge_info i1 i2 =
    match i1, i2 with
    | Loc l1, Loc l2 ->
        Loc {
          fname = l1.fname;
          l_start = l1.l_start; l_end = l2.l_end;
          c_start = l1.c_start; c_end = l2.c_end }
    | _ -> Unknown

  let withinfo2 i1 i2 v = withinfo (merge_info i1 i2) v

  let output_info channel = function
    | Loc { fname; l_start=l1; l_end=l2; c_start=c1; c_end=c2 } ->
        let print_range text a b =
          if a = b
            then fprintf channel "%s %d" text a
            else fprintf channel "%ss %d-%d" text a b
        in
        fprintf channel "%s, " fname;
        print_range "line" l1 l2;
        output_string channel ", ";
        print_range "col" c1 c2;
        output_string channel ":"
    | Unknown ->
        output_string channel "unknown location:"

  let error_at info msg =
    output_info stderr info;
    output_string stderr " ";
    output_string stderr msg;
    output_string stderr "\n";
    exit 1
end