open Printf
open Support.Pervasive

module Code = struct
  type t = cmd list
  and cmd =
    | Add of int
    | Put
    | Get
    | Move of int
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
      | Move n ->
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
    | Move of int
    | Loop of t
  
  let rec from_code code =
    List.map
      (function
        | Code.Add n -> Add n
        | Code.Put -> Put
        | Code.Get -> Get
        | Code.Move n -> Move n
        | Code.Loop l -> Loop (from_code l)
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

    let move tape n =
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
            | Move n ->
                Tape.move tape n
            | Loop l ->
                while Tape.get tape <> 0 do
                  loop l
                done
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