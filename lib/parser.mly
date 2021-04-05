%{
module Lib = struct end (* おまじない *)
open Reusable
%}

%token END
%token PLUS
%token MINUS
%token DOT
%token COMMA
%token RSHIFT LSHIFT  // > <
%token LBRACKET RBRACKET  // [ ]
%token EXCL // !
%token QUES  // ?
%token LBRACE RBRACE // { }
%token COLON
%token AT
%token SEMI
%token CELL
%token PTR
%token LIST LIST_UNLIMITED
%token LPAREN RPAREN
%token <int> INT
%token <char> CHAR
%token <string> VAR

%start program
%type <Program.t> program

%%

program:
  | f=field sl=stmt_list END { (f, sl) }


field:
  | LBRACE el=field_elm_list RBRACE { el }

field_elm_list:
  | e=field_elm SEMI el=field_elm_list { e :: el }
  | { [] }

field_elm:
  | v=VAR COLON ek=field_elm_kind { (v, ek) }

field_elm_kind:
  | CELL { Field.Cell }
  | PTR { Field.Ptr }
  | LIST LPAREN l=INT RPAREN f=field { Field.Lst { length=Some l; mem=f; } }
  | LIST_UNLIMITED f=field { Field.Lst { length=None; mem=f; } }


stmt_list:
  | s=stmt sl=stmt_list { s :: sl }
  | { [] }

stmt:
  | PLUS es=expr ei=expr_opt { StAdd (1, es, ei) }
  | MINUS es=expr ei=expr_opt { StAdd (-1, es, ei) }
  | DOT e=expr { StPut e }
  | COMMA e=expr { StGet e }
  | RSHIFT ep=expr ei=expr_opt { StShift (1, ep, ei) }
  | LSHIFT ep=expr ei=expr_opt { StShift (-1, ep, ei) }
  | EXCL e=expr LBRACKET sl=stmt_list RBRACKET { StWhile (e, sl) }
  | QUES e=expr LBRACKET sl_t=stmt_list RBRACKET LBRACKET sl_e=stmt_list RBRACKET { StIf (e, sl_t, Some sl_e) }

expr:
  | v=VAR { ExVar v }
  | i=INT { ExInt i }
  | c=CHAR { ExInt (int_of_char c) }
  | e=expr COLON v=VAR { ExSelMem (e, None, v) }
  | es=expr COLON LPAREN ei=expr RPAREN v=VAR { ExSelMem (es, Some ei, v) }
  | e=expr AT v=VAR { ExSelPtr (e, v) }

expr_opt:
  | e=expr { Some e }
  | { None }