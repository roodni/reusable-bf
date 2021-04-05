{
let reserved = [
  ("cell", Parser.CELL);
  ("ptr", Parser.PTR);
  ("list", Parser.LIST);
  ("list_unlimited", Parser.LIST_UNLIMITED);
]
}

rule main = parse
  | eof { Parser.END }
  | [' ' '\t' '\n' '\r']+ { main lexbuf }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | "." { Parser.DOT }
  | "," { Parser.COMMA }
  | "<" { Parser.LSHIFT }
  | ">" { Parser.RSHIFT }
  | "[" { Parser.LBRACKET }
  | "]" { Parser.RBRACKET }
  | "!" { Parser.EXCL }
  | "?" { Parser.QUES }
  | "{" { Parser.LBRACE }
  | "}" { Parser.RBRACE }
  | ":" { Parser.COLON }
  | "@" { Parser.AT }
  | ";" { Parser.SEMI }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "0" | ['1'-'9'] ['0'-'9']* {
      Parser.INT (int_of_string @@ Lexing.lexeme lexbuf)
    }
  | "'\\n'" { Parser.CHAR '\n' }
  | "'\\''" { Parser.CHAR '\'' }
  | "'\\\\'" { Parser.CHAR '\\' }
  | "'" [' '-'&' '('-'~'] "'" {
      let c = (Lexing.lexeme lexbuf).[1] in
      Parser.CHAR c
    }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* {
      let id = Lexing.lexeme lexbuf in
      try List.assoc id reserved
      with Not_found -> Parser.VAR id
    }