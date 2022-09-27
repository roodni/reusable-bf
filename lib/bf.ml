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

  let to_buffer code =
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
    buf

  let to_string code = Buffer.contents (to_buffer code)

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
    | While of t ref
    | Wend of t ref
    | ShiftLoop of int
    | MoveLoop of (int * int) list
    | Del

  let from_code code =
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
    let rec rev_convert exe_rev = function
      | [] -> exe_rev
      | cmd :: cmds ->
          let exe_rev =
            match cmd with
            | Code.Add n when n = 0 -> exe_rev
            | Code.Add n -> Add n :: exe_rev
            | Code.Put -> Put :: exe_rev
            | Code.Get -> Get :: exe_rev
            | Code.Shift n when n = 0 -> exe_rev
            | Code.Shift n -> Shift n :: exe_rev
            | Code.Loop [ Shift n ] -> ShiftLoop n :: exe_rev
            | Code.Loop l -> begin
                match move_loop_body l with
                | Some [] -> Del :: exe_rev
                | Some mlb -> MoveLoop mlb :: exe_rev
                | None ->
                    let er = While (ref exe_rev) :: exe_rev in (* refはダミー *)
                    Wend (ref exe_rev) :: rev_convert er l
              end
          in
          rev_convert exe_rev cmds
    in
    let exe_rev = rev_convert [] code in
    let rec rev_construct exe wend_stack = function
      | [] -> assert (wend_stack = []); exe
      | cmd :: exe_rev -> begin
          let wend_stack =
            match cmd, wend_stack with
            | Wend ref_exe_wend, _ -> (ref_exe_wend, exe) :: wend_stack
            | While ref_exe_while, (ref_exe_wend, exe_after_wend) :: wend_stack ->
                ref_exe_while := exe_after_wend;
                ref_exe_wend := exe;
                wend_stack
            | While _, [] -> assert false
            | _ -> wend_stack
          in
          rev_construct (cmd :: exe) wend_stack exe_rev
        end
    in
    rev_construct [] [] exe_rev


  module Dump = struct
    type t = {
      p: int;
      p_max: int;
      cells: int Array.t;
    }

    let dump d =
      let cols_n = 20 in
      let len =
        let len_v =
          (0 -- d.p_max)
            |> List.map
              (fun i ->
                d.cells.(i)
                  |> string_of_int
                  |> String.length )
            |> List.fold_left max 3
        in
        let len_p = d.p_max |> string_of_int |> String.length in
        max len_v len_p
      in
      let rec loop i_left =
        let i_right = min d.p_max (i_left + cols_n - 1) in
        let is_ptr_disp i =
          i = i_left || i = i_right || i mod 5 = 0 || i = d.p
        in
        (* インデックスの出力 *)
        let emph_l, emph_r = '{', '}' in
        (i_left -- i_right) |> List.iter (fun i ->
          let s_of_i =
            if is_ptr_disp i then sprintf "%*d" len i
            else String.repeat " " len
          in
          let partition_left =
            if i = d.p then emph_l
            else if i = d.p + 1 then emph_r
            else ' '
          in
          printf "%c%s" partition_left s_of_i
        );
        if i_right = d.p then printf "%c\n" emph_r
        else printf " \n";
        (* 値の出力 *)
        (i_left -- i_right) |> List.iter (fun i ->
          print_string "|";
          printf "%*d" len d.cells.(i)
        );
        printf "|\n";
        if i_right < d.p_max then
          loop (i_right + 1)
      in
      loop 0;
      flush stdout

    let geti tape i = tape.cells.(i)
  end

  exception Err of string

  let run ~printer ~input ~cell_type executable =
    let tape_size = 300000 in
    let tape = Array.make tape_size 0 in
    let mut_p = ref 0 in
    let mut_p_max = ref 0 in

    let modify_cell_value =
      match cell_type with
        | WrapAround256 -> (fun v -> v land 255)
        | Overflow256 ->
            (fun v -> if 0 <= v && v < 256 then v else raise (Err "Overflow"))
        | OCamlInt -> (fun (v: int) -> v)
    in

    let update_p_max p_new =
      if !mut_p_max < p_new then mut_p_max := p_new
    in

    let rec loop = function
      | [] -> Ok ()
      | cmd :: cmds -> begin
          match cmd with
          | Add n ->
              let p = !mut_p in
              tape.(p) <- modify_cell_value (tape.(p) + n);
              loop cmds
          | Put ->
              let v = tape.(!mut_p) in
              printer (char_of_int v);
              loop cmds
          | Get ->
              let v = match input () with
                | Some c -> int_of_char c
                | None -> raise (Err "End of input")
              in
              tape.(!mut_p) <- modify_cell_value v;
              loop cmds
          | Shift n ->
              let p = !mut_p + n in
              mut_p := p;
              update_p_max p;
              loop cmds
          | While ref_exe ->
              if tape.(!mut_p) = 0
                then loop !ref_exe
                else loop cmds
          | Wend ref_exe ->
              if tape.(!mut_p) = 0
                then loop cmds
                else loop !ref_exe
          | ShiftLoop n ->
              let rec shift_loop l =
                if tape.(l) = 0 then l else shift_loop (l + n)
              in
              let p = shift_loop !mut_p in
              mut_p := p;
              update_p_max p;
              loop cmds
          | MoveLoop mlb ->
              let p = !mut_p in
              let v0 = tape.(p) in
              if v0 <> 0 then begin
                tape.(p) <- 0;
                let rec move_loop l_max = function
                  | [] -> l_max
                  | (offset, coef) :: rest ->
                      let l = p + offset in
                      let l_max = if l_max < l then l else l_max in
                      let v = tape.(l) in
                      tape.(l) <- modify_cell_value (v + v0 * coef);
                      move_loop l_max rest
                in
                let p_max = move_loop !mut_p_max mlb in
                update_p_max p_max;
                loop cmds
              end else
                loop cmds
          | Del ->
              tape.(!mut_p) <- 0;
              loop cmds
        end
    in
    let res =
      try loop executable with
      | Err msg -> Error msg
      | Invalid_argument  _ -> Error "Pointer out of range"
    in
    (res, Dump.{ p = !mut_p; p_max = !mut_p_max; cells = tape; })
  ;;

  let run_stdio ~cell_type executable =
    let flushed = ref true in
    run
      ~printer:(fun c ->
          print_char c;
          flush stdout;
          (* if c = '\n'
            then (flush stdout; flushed := true)
            else flushed := false *)
        )
      ~input:(fun () ->
          if not !flushed then begin
            flush stdout;
            flushed := true
          end;
          try Some (input_char stdin) with
          | End_of_file -> None
        )
      ~cell_type
      executable

  let run_string ~input ~cell_type executable =
    let buf = Buffer.create 100 in
    let res, tape =
      run
        ~printer:(Buffer.add_char buf)
        ~input:(fun () ->
            try Some (Stream.next input) with
            | Stream.Failure -> None
          )
        ~cell_type
        executable
    in
    (res, tape, Buffer.contents buf)
end