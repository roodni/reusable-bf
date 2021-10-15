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

  let rec to_string program =
    program
    |> List.map (function
      | Add n ->
          if n > 0 then
            String.repeat "+" n
          else if n < 0 then
            String.repeat "-" (-n)
          else ""
      | Put -> "."
      | Get -> ","
      | Shift n ->
          if n > 0 then
            String.repeat ">" n
          else if n < 0 then
            String.repeat "<" (-n)
          else ""
      | Loop cmds -> "[" ^ to_string cmds ^ "]"
    )
    |> String.concat ""
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

    let geti_opt cells (i: int) : int option = Some (cells.(i))
    let geti cells (i: int) : int = cells.(i)
    let seti cells (i: int) (n: int) =
      cells.(i) <- n

    let validate_location tape l =
      if l < 0 || Array.length tape.cells <= l then
        raise (Err "Pointer out of range")

    let shift tape n =
      let p = tape.ptr + n in
      validate_location tape p;
      tape.ptr <- p;
      tape.ptr_max <- max tape.ptr_max p

    let set tape n =
      let n = match tape.cell_type with
        | WrapAround256 -> n land 255
        | Overflow256 when 0 <= n && n < 256 -> n
        | Overflow256 -> raise (Err "Overflow")
        | OCamlInt -> n
      in
      seti tape.cells tape.ptr n

    let get tape =
      geti tape.cells tape.ptr

    let shift_loop tape n =
      while get tape <> 0 do
        shift tape n
      done

    let move_loop tape pos_coef_list =
      let v0 = get tape in
      List.iter
        (fun (pos, coef) ->
          let l = tape.ptr + pos in
          validate_location tape l;
          let v = geti tape.cells l in
          seti tape.cells l (v + v0 * coef)
        )
        pos_coef_list;
      set tape 0

    let dump tape =
      let cols_n = 20 in
      let len =
        let len_v =
          (0 -- tape.ptr_max)
          |> List.map
            (fun i ->
              geti_opt tape.cells i
              |> Option.map (fun i -> String.length @@ string_of_int i)
              |> Option.value ~default:0)
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
          match geti_opt tape.cells i with
          | None -> print_string @@ String.repeat " " len
          | Some v -> printf "%*d" len v
        );
        printf "|\n";
        if i_right < tape.ptr_max then
          loop (i_right + 1)
      in
      loop 0;
      flush stdout;
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
      ~printer:(fun c -> print_char c; flush stdout)
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