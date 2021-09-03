{
open Support.Error
module P = Parser

let reserved = [
  ("cell", fun i -> P.CELL i);
  ("ptr", fun i -> P.PTR i);
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
  ("end", fun i -> P.END i);
  ("mod", fun i -> P.MOD i);
  ("true", fun i -> P.TRUE i);
  ("false", fun i -> P.FALSE i);
  ("nil", fun i -> P.NIL i);
]

let string_to_char = function
  | "\\n" -> '\n'
  | "\\t" -> '\t'
  | "\\\\" -> '\\'
  | "\\\'" -> '\''
  | "\\\"" -> '\"'
  | s when String.length s = 1 -> s.[0]
  | _ -> assert false

let info lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  create_info p.pos_fname p.pos_lnum (1 + p.pos_cnum - p.pos_bol)

exception Error of info

let create filename channel =
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf
}


let character =
  [' '-'!' '#'-'[' ']'-'~'] |
  "\\" ['n' 't' '\\' '\'' '\"']


rule str i l = parse
  | character | "\'" {
      let c = Lexing.lexeme lexbuf |> string_to_char in
      str i (c :: l) lexbuf
    }
  | "\n" { Lexing.new_line lexbuf; str i ('\n' :: l) lexbuf }
  | "\\\n" { Lexing.new_line lexbuf; str i l lexbuf }
  | "\"" { P.STRING (i, List.rev l) }
  | _ { raise @@ Error (info lexbuf) }

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
  | "$var" { P.ST_VAR (info lexbuf) }
  | "$let" { P.ST_LET (info lexbuf) }
  | "$dive" { P.ST_DIVE (info lexbuf) }
  | "0" | ['1'-'9'] ['0'-'9']* {
      P.INT (info lexbuf, int_of_string @@ Lexing.lexeme lexbuf)
    }
  | "\'" (character | "\"") "\'" {
      let s = Lexing.lexeme lexbuf in
      let s = String.sub s 1 (String.length s - 2) in
      P.CHAR (info lexbuf, string_to_char s)
    }
  | "\"" { str (info lexbuf) [] lexbuf }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* {
      let id = Lexing.lexeme lexbuf in
      try (List.assoc id reserved) (info lexbuf)
      with Not_found -> P.VAR (info lexbuf, id)
    }
  | _ { raise @@ Error (info lexbuf) }