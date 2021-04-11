{
module P = Parser

let reserved = [
  ("cell", P.CELL);
  ("ptr", P.PTR);
  ("list", P.LIST);
  ("list_unlimited", P.LIST_UNLIMITED);
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
}

rule main = parse
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
  | "'\\n'" { P.CHAR '\n' }
  | "'\\''" { P.CHAR '\'' }
  | "'\\\\'" { P.CHAR '\\' }
  | "'" [' '-'&' '('-'~'] "'" {
      let c = (Lexing.lexeme lexbuf).[1] in
      P.CHAR c
    }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* {
      let id = Lexing.lexeme lexbuf in
      try List.assoc id reserved
      with Not_found -> P.VAR id
    }