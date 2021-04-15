{
module P = Parser

let reserved = [
  ("cell", P.CELL);
  ("ptr", P.PTR);
  ("array", P.ARRAY);
  ("array_unlimited", P.ARRAY_UNLIMITED);
  ("main", P.MAIN);
  ("fun", P.FUN);
  ("if", P.IF);
  ("then", P.THEN);
  ("else", P.ELSE);
  ("let", P.LET);
  ("in", P.IN);
  ("match", P.MATCH);
  ("with", P.WITH);
  ("end", P.END);
  ("mod", P.MOD);
  ("true", P.TRUE);
  ("false", P.FALSE);
  ("nil", P.NIL);
]

let string_to_char = function
  | "\\n" -> '\n'
  | "\\t" -> '\t'
  | "\\\\" -> '\\'
  | "\\\'" -> '\''
  | "\\\"" -> '\"'
  | s when String.length s = 1 -> s.[0]
  | _ -> assert false

}

let character =
  [' '-'!' '#'-'[' ']'-'~'] |
  "\\" ['n' 't' '\\' '\'' '\"']


rule str l = parse
  | character | "\'" {
      let c = Lexing.lexeme lexbuf |> string_to_char in
      str (c :: l) lexbuf
    }
  | "\n" { Lexing.new_line lexbuf; str ('\n' :: l) lexbuf }
  | "\\\n" { Lexing.new_line lexbuf; str l lexbuf }
  | "\"" { P.STRING (List.rev l) }

and main = parse
  | eof { P.EOF }
  | [' ' '\t' '\r']+ { main lexbuf }
  | "\n" { Lexing.new_line lexbuf; main lexbuf }
  | "#" [^'\n']* { main lexbuf }
  | "+" { P.PLUS }
  | "-" { P.MINUS }
  | "." { P.DOT }
  | "," { P.COMMA }
  | "<" { P.LSHIFT }
  | ">" { P.RSHIFT }
  | "[" { P.LBRACKET }
  | "]" { P.RBRACKET }
  | "!" { P.EXCL }
  | "?" { P.QUES }
  | "{" { P.LBRACE }
  | "}" { P.RBRACE }
  | ":" { P.COLON }
  | "@" { P.AT }
  | ";" { P.SEMI }
  | "(" { P.LPAREN }
  | ")" { P.RPAREN }
  | "=" { P.EQ }
  | "*" { P.ASTER }
  | "/" { P.SLASH }
  | "->" { P.ARROW }
  | "<=" { P.LEQ }
  | "::" { P.CONS }
  | "|" { P.BAR }
  | "$var" { P.ST_VAR }
  | "$let" { P.ST_LET }
  | "$dive" { P.ST_DIVE }
  | "0" | ['1'-'9'] ['0'-'9']* {
      P.INT (int_of_string @@ Lexing.lexeme lexbuf)
    }
  | "\'" (character | "\"") "\'" {
      let s = Lexing.lexeme lexbuf in
      let s = String.sub s 1 (String.length s - 2) in
      P.CHAR (string_to_char s)
    }
  | "\"" { str [] lexbuf }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* {
      let id = Lexing.lexeme lexbuf in
      try List.assoc id reserved
      with Not_found -> P.VAR id
    }