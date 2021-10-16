open Printf
open Support.Pervasive

module Code = struct
  type t = cmd list
  and cmd =
    | Add of int
    | Put
    | Get
    | Shift of int
    | Loop of t

  let to_string code =
    let buf = Buffer.create 10000 in
    let rec loop code =
      List.iter
        (function
          | Add n ->
              let c = if n < 0 then '-' else '+' in
              (1 -- abs n) |> List.iter (fun _ -> Buffer.add_char buf c)
          | Put -> Buffer.add_char buf '.'
          | Get -> Buffer.add_char buf ','
          | Shift n ->
              let c = if n < 0 then '<' else '>' in
              (1 -- abs n) |> List.iter (fun _ -> Buffer.add_char buf c)
          | Loop l ->
              Buffer.add_char buf '[';
              loop l;
              Buffer.add_char buf ']'
        )
        code
    in
    loop code;
    Buffer.contents buf
  ;;

  exception ParseError

  let parse stream =
    let lookahead () = Stream.peek stream in
    let consume () = Stream.junk stream in
    let expect c =
      if lookahead () <> Some c then raise ParseError;
      consume ()
    in
    let rec parse ~nested =
      let code_rev = ref [] in
      let append cmd =
        code_rev := cmd :: !code_rev
      in
      let is_cmd = function
        | '+' | '-' | '>' | '<' | '.' | ',' | '[' | ']' -> true
        | _ -> false
      in
      let rec loop () =
        match lookahead () with
        | None -> ()
        | Some ('+' | '-') -> loop_adding 0
        | Some ('>' | '<') -> loop_shifting 0
        | Some '.' -> consume (); append Put; loop ()
        | Some ',' -> consume (); append Get; loop ()
        | Some '[' ->
            consume ();
            let code = parse ~nested:true in
            expect ']';
            append (Loop code);
            loop ()
        | Some ']' -> if not nested then raise ParseError
        | Some _ -> consume (); loop ()
      and loop_adding n =
        match lookahead () with
        | Some '+' -> consume (); loop_adding (n + 1)
        | Some '-' -> consume (); loop_adding (n - 1)
        | Some c when not (is_cmd c) -> consume (); loop_adding n
        | _ -> append (Add n); loop ()
      and loop_shifting n =
        match lookahead () with
        | Some '>' -> consume (); loop_shifting (n + 1)
        | Some '<' -> consume (); loop_shifting (n - 1)
        | Some c when not (is_cmd c) -> consume (); loop_shifting n
        | _ -> append (Shift n); loop ()
      in
      loop ();
      List.rev !code_rev
    in
    parse ~nested:false
end

type cell_type = WrapAround256 | Overflow256 | OCamlInt


module Exe = struct
  type t = cmd list
  and cmd =
    | Add of int
    | Put
    | Get
    | Shift of int
    | Loop of t
    | ShiftLoop of int
    | MoveLoop of (int * int) list

  let rec from_code code =
    let move_loop_body code =
      let tbl : (int, int) Hashtbl.t = Hashtbl.create 3 in
      let pos = ref 0 in
      try
        List.iter
          (function
            | Code.Add n ->
                let a = Hashtbl.find_opt tbl !pos |> Option.value ~default:0 in
                Hashtbl.replace tbl !pos (a + n)
            | Code.Shift n -> pos := !pos + n
            | _ -> raise Exit)
          code;
        if !pos <> 0 then raise Exit;
        if Hashtbl.find_opt tbl 0 <> Some (-1) then raise Exit;
        Hashtbl.remove tbl 0;
        Hashtbl.to_seq tbl |> List.of_seq |> Option.some
      with Exit -> None
    in
    List.map
      (function
        | Code.Add n -> Add n
        | Code.Put -> Put
        | Code.Get -> Get
        | Code.Shift n -> Shift n
        | Code.Loop [ Shift n ] -> ShiftLoop n
        | Code.Loop l -> begin
            match move_loop_body l with
            (* match None with *)
            | Some mlb -> MoveLoop mlb
            | None -> Loop (from_code l)
          end
      )
      code

  exception Err of string

  module Tape = struct
    type t =
      { mutable ptr: int;
        mutable ptr_max: int;
        cells: int Array.t;
        cell_type: cell_type;
      }

    let init cell_type =
      { ptr = 0;
        ptr_max = 0;
        cells = Array.make 100000 0;
        cell_type;
      }

    let validate_location tape l =
      if l < 0 || Array.length tape.cells <= l then
        raise (Err "Pointer out of range")

    let shift tape n =
      let p = tape.ptr + n in
      validate_location tape p;
      tape.ptr <- p;
      if tape.ptr_max < p then tape.ptr_max <- p

    let modify_cell_value tape v =
      match tape.cell_type with
        | WrapAround256 -> v land 255
        | Overflow256 when 0 <= v && v < 256 -> v
        | Overflow256 -> raise (Err "Overflow")
        | OCamlInt -> v

    let set tape n =
      tape.cells.(tape.ptr) <- modify_cell_value tape n

    let get tape = tape.cells.(tape.ptr)

    let shift_loop tape n =
      while get tape <> 0 do
        shift tape n
      done

    let move_loop tape pos_coef_list =
      let v0 = get tape in
      if v0 <> 0 then begin
        List.iter
          (fun (pos, coef) ->
            let l = tape.ptr + pos in
            validate_location tape l;
            let v = tape.cells.(l) in
            tape.cells.(l) <- modify_cell_value tape (v + v0 * coef)
          )
          pos_coef_list;
        set tape 0
      end

    let dump tape =
      let cols_n = 20 in
      let len =
        let len_v =
          (0 -- tape.ptr_max)
            |> List.map
              (fun i ->
                tape.cells.(i)
                  |> string_of_int
                  |> String.length )
            |> List.fold_left max 3
        in
        let len_p = tape.ptr_max |> string_of_int |> String.length in
        max len_v len_p
      in
      let rec loop i_left =
        let i_right = min tape.ptr_max (i_left + cols_n - 1) in
        let is_ptr_disp i =
          i = i_left || i = i_right || i mod 5 = 0 || i = tape.ptr
        in
        (* インデックスの出力 *)
        let emph_l, emph_r = '{', '}' in
        (i_left -- i_right) |> List.iter (fun i ->
          let s_of_i =
            if is_ptr_disp i then sprintf "%*d" len i
            else String.repeat " " len
          in
          let partition_left =
            if i = tape.ptr then emph_l
            else if i = tape.ptr + 1 then emph_r
            else ' '
          in
          printf "%c%s" partition_left s_of_i
        );
        if i_right = tape.ptr then printf "%c\n" emph_r
        else printf " \n";
        (* 値の出力 *)
        (i_left -- i_right) |> List.iter (fun i ->
          print_string "|";
          printf "%*d" len tape.cells.(i)
        );
        printf "|\n";
        if i_right < tape.ptr_max then
          loop (i_right + 1)
      in
      loop 0;
      flush stdout

    let geti tape i = tape.cells.(i)
  end

  let run ~printer ~input ~cell_type executable =
    let tape = Tape.init cell_type in
    let rec loop = function
      | [] -> ()
      | cmd :: cmds -> begin
          let () = match cmd with
            | Add n ->
                let v = Tape.get tape in
                Tape.set tape (v + n)
            | Put ->
                let v = Tape.get tape in
                printer (char_of_int v)
            | Get ->
                let c =
                  try Stream.next input with
                  | Stream.Failure -> raise (Err "End of input")
                in
                Tape.set tape (int_of_char c)
            | Shift n ->
                Tape.shift tape n
            | Loop l ->
                while Tape.get tape <> 0 do
                  loop l
                done
            | ShiftLoop n -> Tape.shift_loop tape n
            | MoveLoop mlb -> Tape.move_loop tape mlb
          in
          loop cmds
        end
    in
    let res =
      try loop executable; Ok () with
      | Err msg -> Error msg
    in
    (res, tape)

  let run_stdio ~cell_type executable =
    run
      ~printer:(fun c ->
          print_char c;
          if c = '\n' then flush stdout
        )
      ~input:(Stream.of_channel stdin)
      ~cell_type
      executable

  let run_string ~input ~cell_type executable =
    let buf = Buffer.create 100 in
    let res, tape =
      run
        ~printer:(Buffer.add_char buf)
        ~input ~cell_type
        executable
    in
    (res, tape, Buffer.contents buf)
end