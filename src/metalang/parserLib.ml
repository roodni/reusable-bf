open Parser

(* これ他に方法はなかったのか？しかし思いつかん *)
let info_of_token = function
  | INT w -> w.i
  | CHAR w -> w.i
  | STRING w -> w.i
  | VAR w -> w.i
  | UVAR w -> w.i
  | EOF i
  | PLUS i | MINUS i -> i
  | DOT i | COMMA i | RSHIFT i | LSHIFT i -> i
  | LBRACKET i | RBRACKET i | EXCL i | QUES i -> i
  | LBRACE i | RBRACE i | COLON i | COLONCOLON i -> i
  | AT i | ATAT i | PIPE i | SEMI i | SEMISEMI i -> i
  | LPAREN i | RPAREN i | EQ i | NEQ i | LEQ i | GEQ i -> i
  | ASTER i | ASTER2 i | SLASH i | ARROW i -> i
  | UNDER i | BAR i | BARBAR i | ANDAND i | MOD i -> i
  | INDEX_LOOP i | INDEX_IF i -> i
  | ST i | ST_ALLOC i | ST_BUILD i | ST_DIVE i -> i
  | CELL i | INDEX i | ARRAY i -> i
  | FUN i | FUNCTION i | LET i | IN i | REC i -> i
  | IF i | THEN i | ELSE i | MATCH i | WITH i -> i
  | PRIVATE i | IMPORT i | MODULE i | OPEN i -> i
  | INCLUDE i | STRUCT i | END i | BEGIN i -> i
  | TRUE i | FALSE i | NIL i -> i
