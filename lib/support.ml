module Error = struct
  type info =
    | Loc of { file : string; line : int; col : int }
    | Unknown

end