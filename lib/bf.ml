open Batteries
open Printf


module Cmd = struct
  type t =
    | Add of int
    | Put
    | Get
    | Move of int
    | Loop of t list
    | Dump
    | Comment of string

  let rec list_to_string program =
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
      | Loop cmds -> "[" ^ list_to_string cmds ^ "]"
      | Dump -> "#"
      | Comment s ->
          let s =
            s |> String.enum |>
            filter (fun c -> not @@ List.mem c ['+'; '-'; '.'; ','; '>'; '<'; '['; ']']) |>
            String.of_enum
          in
          "\n" ^ s ^ "\n"
    )
    |> String.concat ""
end


module Err = struct
  type t =
    | End_of_input
    | Overflow
    | Ptr_out_of_range

  let to_string = function
    | End_of_input -> "end of input"
    | Overflow -> "overflow"
    | Ptr_out_of_range -> "pointer out of range"

  let opt_to_string = function
    | None -> "ok"
    | Some e -> to_string e
end


module Tape = struct
  type t = {
    ptr: int;
    ptr_max: int;
    array: int Array.t;
  }

  let init () = {
    ptr = 0;
    ptr_max = 0;
    array = Array.make 100000 0;
  }

  let move n tape =
    let { ptr; ptr_max; array } = tape in
    let ptr = ptr + n in
    let ptr_max = max ptr ptr_max in
    if 0 <= ptr && ptr < Array.length array then
      Ok { tape with ptr; ptr_max }
    else
      Error Err.Ptr_out_of_range

  let set n tape =
    if 0 <= n && n < 256 then
      let { ptr; array; _ } = tape in
      array.(ptr) <- n;
      Ok tape
    else
      Error Err.Overflow

  let get tape =
    let { ptr; array; _ } = tape in
    array.(ptr)

  let geti i tape =
    let { array; _ } = tape in
    array.(i)

  let dump tape =
    let cols_n = 20 in
    let len =
      let len_v =
        (0 -- tape.ptr_max) |>
        map (fun i -> geti i tape |> string_of_int |> String.length) |>
        fold max 3
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
      (i_left -- i_right) |> iter (fun i ->
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
      (i_left -- i_right) |> iter (fun i ->
        printf "|%*d" len (geti i tape)
      );
      printf "|\n";
      if i_right < tape.ptr_max then
        loop (i_right + 1)
    in
    loop 0;
    flush stdout;
end


module State = struct
  type t = {
    stack: Cmd.t list list;
    tape: Tape.t;
    input: char Enum.t;
    output: int list;
    err: Err.t option;
    comments: string list;
  }

  let init ~program ~input =
    {
      stack = [ program ];
      tape = Tape.init ();
      input;
      output = [];
      err = None;
      comments = [];
    }

  let output_to_string { output; _ } =
    output |> List.rev_map (fun i -> char_of_int i) |> String.of_list

  let is_finished { stack; err; _ } =
    err <> None || List.is_empty stack

  let step ?(printer=fun (_: char) -> ()) ({ stack; tape; input; output; comments; _; } as state) =
    if is_finished state then state
    else
      match stack with
      | [] -> state
      | [] :: stack_rest -> { state with stack = stack_rest; }
      | (cmd :: cmds_rest) :: stack_rest -> begin
          let stack = cmds_rest :: stack_rest in
          match cmd with
          | Cmd.Add n -> begin
              let res = Tape.set (Tape.get tape + n) tape in
              match res with
              | Ok tape -> { state with stack; tape; }
              | Error err -> { state with err = Some err; }
            end
          | Cmd.Put ->
              let x = Tape.get tape in
              printer @@ char_of_int x;
              let output = x :: output in
              { state with stack; output }
          | Cmd.Get -> begin
              match Enum.get input with
              | None -> { state with err = Some Err.End_of_input; }
              | Some c -> begin
                  let res = Tape.set (int_of_char c) tape in
                  match res with
                  | Ok tape -> { state with stack; tape; }
                  | Error err -> { state with err = Some err; }
                end
            end
          | Cmd.Move n -> begin
              match Tape.move n tape with
              | Ok tape -> { state with stack; tape; }
              | Error err -> { state with err = Some err; }
            end
          | Cmd.Loop loop -> begin
              let x = Tape.get tape in
              if x = 0 then
                { state with stack; }
              else
                { state with stack = loop :: (cmd :: cmds_rest) :: stack_rest }
            end
          | Dump ->
              print_newline ();
              Tape.dump tape;
              { state with stack }
          | Comment s ->
              { state with stack; comments = s :: comments; }
        end

  let dump state =
    printf "--- state dump ---\n";
    print_endline "[output]";
    output_to_string state |> print_endline;
    print_endline "[tape]";
    Tape.dump state.tape;
    print_endline "[comments]";
    state.comments |> List.take 100 |> String.concat " <- " |> print_endline;
    print_endline "[error]";
    print_endline (Err.opt_to_string state.err);
    flush stdout
end

let cnt = ref 0

let run ?printer program input =
  cnt := 0;
  let rec loop state =
    incr cnt;
    (* assert (!cnt < 100000); *)
    if State.is_finished state then
      state
    else
      loop (State.step ?printer state)
  in
  let state = State.init ~program ~input in
  loop state


let run_stdio program =
  let printer c = printf "%c%!" c in
  run ~printer program (input_chars Stdlib.stdin)