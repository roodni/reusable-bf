{
open Support.Info
module P = Parser

let reserved = [
  ("codegen", fun i -> P.CODEGEN i);
  ("import", fun i -> P.IMPORT i);
  ("as", fun i -> P.AS i);
  ("cell", fun i -> P.CELL i);
  ("index", fun i -> P.INDEX i);
  ("array", fun i -> P.ARRAY i);
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

(** [Support.Info]の[create_info]を隠蔽
    info作成と同時にcurr_infoを書きかえる *)
let create_info p1 p2 =
  let i = Support.Info.create_info p1 p2 in
  curr_info := i;
  i

let info lexbuf =
  let p1 = Lexing.lexeme_start_p lexbuf in
  let p2 = Lexing.lexeme_end_p lexbuf in
  create_info p1 p2

let create filename channel =
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf
}


let character =
  [' '-'!' '#'-'[' ']'-'~'] |
  "\\" ['n' 't' '\\' '\'' '\"']


rule comment = parse
  | "(*" { comment (comment lexbuf) }
  | "*)" { lexbuf }
  | "\n" { Lexing.new_line lexbuf; comment lexbuf }
  | eof { Error.at (info lexbuf) Lexer_Unexpected }
  | _ { comment lexbuf }

and str p1 l = parse
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
  | eof | _ { Error.at (info lexbuf) Lexer_Unexpected }

and main = parse
  | [' ' '\t' '\r']+ { main lexbuf }
  | "\n" { Lexing.new_line lexbuf; main lexbuf }
  | "//" [^'\n']* { main lexbuf }
  | "(*" { main (comment lexbuf) }
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
  | "@@" { P.ATAT (info lexbuf) }
  | ";" { P.SEMI (info lexbuf) }
  | "(" { P.LPAREN (info lexbuf) }
  | ")" { P.RPAREN (info lexbuf) }
  | "=" { P.EQ (info lexbuf) }
  | "*" { P.ASTER (info lexbuf) }
  | "**" { P.ASTER2 (info lexbuf) }
  | "/" { P.SLASH (info lexbuf) }
  | "->" { P.ARROW (info lexbuf) }
  | "<=" { P.LEQ (info lexbuf) }
  | "|" { P.BAR (info lexbuf) }
  | "$" { P.ST (info lexbuf) }
  | "$alloc" { P.ST_ALLOC (info lexbuf) }
  | "$build" { P.ST_BUILD (info lexbuf) }
  | "$dive" { P.ST_DIVE (info lexbuf) }
  | "$iloop" { P.ST_ILOOP (info lexbuf) }
  | "0" | ['1'-'9'] ['0'-'9']* {
      match int_of_string_opt (Lexing.lexeme lexbuf) with
      | Some i -> P.INT (withinfo (info lexbuf) i)
      | None -> Error.at (info lexbuf) Lexer_Too_large_int
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
      with Not_found -> P.VAR (withinfo (info lexbuf) (Syntax.Var.of_string id))
    }
  | ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* {
      let id = Lexing.lexeme lexbuf in
      P.UVAR (withinfo (info lexbuf) (Syntax.UVar.of_string id))
    }
  | _ { Error.at (info lexbuf) Lexer_Unexpected }