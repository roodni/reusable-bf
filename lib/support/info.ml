open Format

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
  let (f1, l1, c1) = position_to_flc pos1 in
  let (f2, l2, c2) = position_to_flc pos2 in
  assert (f1 = f2);
  Loc {
    fname = f1;
    l_start = l1;
    l_end = l2;
    c_start = c1;
    c_end =
      if (l1, c1) = (l2, c2) then c1 else c2 - 1;
  }

let withinfo i v = { i; v; }
let clearinfo { v; _ } = v

let merge_info i1 i2 =
  match i1, i2 with
  | Loc l1, Loc l2 ->
      Loc {
        fname = l1.fname;
        l_start = l1.l_start; l_end = l2.l_end;
        c_start = l1.c_start; c_end = l2.c_end
      }
  | _ -> Unknown

let withinfo2 i1 i2 v = withinfo (merge_info i1 i2) v

let output_info ppf = function
  | Loc { fname; l_start=l1; l_end=l2; c_start=c1; c_end=c2 } ->
      let print_range text a b =
        if a = b
          then fprintf ppf "%s %d" text a
          else fprintf ppf "%ss %d-%d" text a b
      in
      if fname <> "" then
        fprintf ppf "File \"%s\", " fname;
      print_range "line" l1 l2;
      fprintf ppf ", ";
      print_range "col" c1 c2;
      fprintf ppf ":@ ";
  | Unknown ->
      ();
;;

let lines_of_info = function
  | Loc { l_start; l_end; _ } -> Some (l_start, l_end)
  | Unknown -> None


type trace = Trace of {
    tailcalln : int;
    stack : (info * int) list;
  }

let empty_trace = Trace {tailcalln=0; stack=[]}

let push_tailcall (Trace t) =
  Trace { t with tailcalln=t.tailcalln + 1 }

let push_info info (Trace t) =
  Trace {
    tailcalln = 0;
    stack = (info, t.tailcalln) :: t.stack;
  }

let output_trace ppf (Trace t) =
  assert (t.tailcalln = 0);
  fprintf ppf "@[<v>Traceback:@,";
  fprintf ppf "  @[<v>";
  let rec dump = function
    | [] -> ()
    | (info, ommited) :: tl ->
        if ommited > 0 then
          fprintf ppf "[Omitted tail calls (%d)]@," ommited;
        output_info ppf info;
        let rec repeat n = function
          | (info', 0) :: tl when info == info' ->
              repeat (n + 1) tl
          | l -> (n, l)
        in
        let repeatn, tl = repeat 0 tl in
        if repeatn > 0 then
          fprintf ppf "  [Repeated (%d)]@," (repeatn + 1);
        dump tl
  in
  dump (List.rev t.stack);
  fprintf ppf "@]@]@,";
;;

let top_of_trace (Trace t) =
  assert (t.tailcalln = 0);
  match t.stack with
  | (info, _) :: _ -> info
  | _ -> assert false