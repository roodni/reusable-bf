(* open Support.Pervasive *)

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
            for _ = 1 to abs n do
              Buffer.add_char buf c
            done
        | Put -> Buffer.add_char buf '.'
        | Get -> Buffer.add_char buf ','
        | Shift n ->
            let c = if n < 0 then '<' else '>' in
            for _ = 1 to abs n do
              Buffer.add_char buf c
            done
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