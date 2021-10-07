{
open Support.Error
module P = Parser

let reserved = [
  ("import", fun i -> P.IMPORT i);
  ("as", fun i -> P.AS i);
  ("cell", fun i -> P.CELL i);
  ("ptr", fun i -> P.PTR i);
  ("index", fun i -> P.PTR i);
  ("array", fun i -> P.ARRAY i);
  ("array_unlimited", fun i -> P.ARRAY_UNLIMITED i);
  ("main", fun i -> P.MAIN i);
  ("fun", fun i -> P.FUN i);
  ("if", fun i -> P.IF i);
  ("then", fun i -> P.THEN i);
  ("else", fun i -> P.ELSE i);
  ("let", fun i -> P.LET i);
  ("in", fun i -> P.IN i);
  ("match", fun i -> P.MATCH i);
  ("with", fun i -> P.WITH i);
  ("mod", fun i -> P.MOD i);
  ("true", fun i -> P.TRUE i);
  ("false", fun i -> P.FALSE i);
  ("nil", fun i -> P.NIL i);
  ("_", fun i -> P.UNDER i)
]

let string_to_char = function
  | "\\n" -> '\n'
  | "\\t" -> '\t'
  | "\\\\" -> '\\'
  | "\\\'" -> '\''
  | "\\\"" -> '\"'
  | s when String.length s = 1 -> s.[0]
  | _ -> assert false


let curr_info = ref unknown_info

(** [Support.Error]の[create_info]を隠蔽
    info作成と同時にcurr_infoを書きかえる *)
let create_info p1 p2 =
  let i = Support.Error.create_info p1 p2 in
  curr_info := i;
  i

let info lexbuf =
  let p1 = Lexing.lexeme_start_p lexbuf in
  let p2 = Lexing.lexeme_end_p lexbuf in
  create_info p1 p2

exception Error of info

let create filename channel =
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf
}


let character =
  [' '-'!' '#'-'[' ']'-'~'] |
  "\\" ['n' 't' '\\' '\'' '\"']


rule str p1 l = parse
  | character | "\'" {
      let c = Lexing.lexeme lexbuf |> string_to_char in
      str p1 (c :: l) lexbuf
    }
  | "\n" { Lexing.new_line lexbuf; str p1 ('\n' :: l) lexbuf }
  | "\\\n" { Lexing.new_line lexbuf; str p1 l lexbuf }
  | "\"" {
      let p2 = Lexing.lexeme_end_p lexbuf in
      let i = create_info p1 p2 in
      let s = List.rev l |> List.to_seq |> String.of_seq in
      P.STRING (withinfo i s)
    }
  | eof | _ { raise @@ Error (info lexbuf) }

and main = parse
  | [' ' '\t' '\r']+ { main lexbuf }
  | "\n" { Lexing.new_line lexbuf; main lexbuf }
  | "#" [^'\n']* { main lexbuf }
  | eof { P.EOF (info lexbuf) }
  | "+" { P.PLUS (info lexbuf) }
  | "-" { P.MINUS (info lexbuf) }
  | "." { P.DOT (info lexbuf) }
  | "," { P.COMMA (info lexbuf) }
  | "<" { P.LSHIFT (info lexbuf) }
  | ">" { P.RSHIFT (info lexbuf) }
  | "[" { P.LBRACKET (info lexbuf) }
  | "]" { P.RBRACKET (info lexbuf) }
  | "!" { P.EXCL (info lexbuf) }
  | "?" { P.QUES (info lexbuf) }
  | "{" { P.LBRACE (info lexbuf) }
  | "}" { P.RBRACE (info lexbuf) }
  | ":" { P.COLON (info lexbuf) }
  | "@" { P.AT (info lexbuf) }
  | ";" { P.SEMI (info lexbuf) }
  | "(" { P.LPAREN (info lexbuf) }
  | ")" { P.RPAREN (info lexbuf) }
  | "=" { P.EQ (info lexbuf) }
  | "*" { P.ASTER (info lexbuf) }
  | "/" { P.SLASH (info lexbuf) }
  | "->" { P.ARROW (info lexbuf) }
  | "<=" { P.LEQ (info lexbuf) }
  | "::" { P.CONS (info lexbuf) }
  | "|" { P.BAR (info lexbuf) }
  | "$var" | "$alloc" { P.ST_VAR (info lexbuf) }
  | "$let" { P.ST_LET (info lexbuf) }
  | "$dive" { P.ST_DIVE (info lexbuf) }
  | "0" | ['1'-'9'] ['0'-'9']* {
      let i = int_of_string @@ Lexing.lexeme lexbuf in
      P.INT (withinfo (info lexbuf) i)
    }
  | "\'" (character | "\"") "\'" {
      let s = Lexing.lexeme lexbuf in
      let c = String.sub s 1 (String.length s - 2) |> string_to_char in
      P.CHAR (withinfo (info lexbuf) c)
    }
  | "\"" {
      let p1 = Lexing.lexeme_start_p lexbuf in
      str p1 [] lexbuf
    }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* {
      let id = Lexing.lexeme lexbuf in
      try (List.assoc id reserved) (info lexbuf)
      with Not_found -> P.VAR (withinfo (info lexbuf) (Reusable.Var.of_string id))
    }
  | ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* {
      let id = Lexing.lexeme lexbuf in
      P.UVAR (withinfo (info lexbuf) (Reusable.UVar.of_string id))
    }
  | _ { raise @@ Error (info lexbuf) }