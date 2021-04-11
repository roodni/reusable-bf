%{
module Lib = struct end (* おまじない *)
open Reusable
%}

%token EOF
%token PLUS MINUS
%token DOT COMMA
%token RSHIFT LSHIFT  // > <
%token LBRACKET RBRACKET  // [ ]
%token EXCL QUES  // ！ ？
%token LBRACE RBRACE // { }
%token COLON
%token AT
%token SEMI
%token LPAREN RPAREN
%token EQ
%token ASTER
%token SLASH
%token ARROW  // ->
%token LEQ

%token ST_VAR
%token ST_LET
%token ST_DIVE

%token CELL PTR LIST LIST_UNLIMITED
%token FUN
%token LET IN
%token IF THEN ELSE
%token MAIN
%token MOD

%token <int> INT
%token <char> CHAR
%token <string> VAR
%token TRUE FALSE

%nonassoc prec_if prec_fun prec_let
%left LSHIFT LEQ EQ
%left PLUS MINUS
%left ASTER SLASH MOD

%start program
%type <Program.t> program

%%

program:
  | MAIN f=field IN sl=stmt_list EOF { (f, sl) }


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


var_list:
  | v=VAR vl=var_list { v :: vl }
  | { [] }


stmt_list:
  | s=stmt sl=stmt_list { s :: sl }
  | ST_VAR f=field IN sl=stmt_list { [ StVar (f, sl) ] }
  | ST_LET v=VAR EQ e=expr_full IN sl=stmt_list { [ StLet (v, e, sl) ] }
  | { [] }

stmt:
  | PLUS es=expr ei=expr_opt { StAdd (1, es, ei) }
  | MINUS es=expr ei=expr_opt { StAdd (-1, es, ei) }
  | DOT e=expr { StPut e }
  | COMMA e=expr { StGet e }
  | RSHIFT ep=expr ei=expr_opt { StShift (1, ep, ei) }
  | LSHIFT ep=expr ei=expr_opt { StShift (-1, ep, ei) }
  | EXCL e=expr LBRACKET sl=stmt_list RBRACKET { StWhile (e, sl) }
  | QUES e=expr LBRACKET sl_t=stmt_list RBRACKET LBRACKET sl_e=stmt_list RBRACKET {
      StIf (e, sl_t, Some sl_e)
    }
  | ASTER e=expr_appable { StExpand e }
  | ST_DIVE e=expr LBRACKET sl=stmt_list RBRACKET { StDive (e, sl) }

expr:
  | v=VAR { ExVar v }
  | i=INT { ExInt i }
  | c=CHAR { ExInt (int_of_char c) }
  | TRUE { ExBool true }
  | FALSE { ExBool false }
  | e=expr COLON v=VAR { ExSelMem (e, None, v) }
  | es=expr COLON LPAREN ei=expr_full RPAREN v=VAR { ExSelMem (es, Some ei, v) }
  | e=expr AT v=VAR { ExSelPtr (e, v) }
  | LBRACKET sl=stmt_list RBRACKET { ExBlock sl }
  | LPAREN e=expr_full RPAREN { e }

expr_opt:
  | e=expr { Some e }
  | { None }

expr_full:
  | e=expr_appable { e }
  | FUN v=VAR vl=var_list ARROW e=expr_full {
      let e = List.fold_right (fun v e -> ExFun (v, e)) vl e in
      ExFun (v, e)
    } %prec prec_fun
  | IF ec=expr_full THEN et=expr_full ELSE ee=expr_full { ExIf (ec, et, ee) } %prec prec_if
  | LET v=VAR EQ e1=expr_full IN e2=expr_full { ExLet(v, e1, e2) } %prec prec_let
  | MINUS e=expr_full { ExMinus e }
  | el=expr_full bop=bop_int er=expr_full { ExBOpInt (el, bop, er) }
  | el=expr_full EQ er=expr_full { ExEqual (el, er) }

%inline bop_int:
  | PLUS { BOpInt.Add }
  | MINUS { BOpInt.Sub }
  | ASTER { BOpInt.Mul }
  | SLASH { BOpInt.Div }
  | MOD { BOpInt.Mod }
  | LSHIFT { BOpInt.Lt }
  | LEQ { BOpInt.Leq }

expr_appable:
  | e=expr { e }
  | ef=expr_appable ea=expr { ExApp (ef, ea) }