%{
module Lib = struct end (* おまじない *)
(* open Support.Error *)
open Reusable
%}

%token <Support.Error.info> EOF
%token <Support.Error.info> PLUS MINUS
%token <Support.Error.info> DOT COMMA
%token <Support.Error.info> RSHIFT LSHIFT  // > <
%token <Support.Error.info> LBRACKET RBRACKET  // [ ]
%token <Support.Error.info> EXCL QUES  // ！ ？
%token <Support.Error.info> LBRACE RBRACE // { }
%token <Support.Error.info> COLON
%token <Support.Error.info> AT
%token <Support.Error.info> SEMI
%token <Support.Error.info> LPAREN RPAREN
%token <Support.Error.info> EQ
%token <Support.Error.info> ASTER
%token <Support.Error.info> SLASH
%token <Support.Error.info> ARROW  // ->
%token <Support.Error.info> LEQ  // <=
%token <Support.Error.info> CONS
%token <Support.Error.info> BAR

%token <Support.Error.info> ST_VAR
%token <Support.Error.info> ST_LET
%token <Support.Error.info> ST_DIVE

%token <Support.Error.info> CELL PTR ARRAY ARRAY_UNLIMITED
%token <Support.Error.info> FUN
%token <Support.Error.info> LET IN
%token <Support.Error.info> IF THEN ELSE
%token <Support.Error.info> MATCH WITH END
%token <Support.Error.info> MAIN
%token <Support.Error.info> MOD

%token <Support.Error.info * int> INT
%token <Support.Error.info * char> CHAR
%token <Support.Error.info * char list> STRING
%token <Support.Error.info * string> VAR
%token <Support.Error.info> TRUE FALSE
%token <Support.Error.info> NIL

%nonassoc prec_if prec_fun prec_let
%left COMMA
%left LSHIFT LEQ EQ
%right CONS
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
  | v=VAR COLON ek=field_elm_kind { (snd v, ek) }

field_elm_kind:
  | CELL { Field.Cell }
  | PTR { Field.Ptr }
  | ARRAY LPAREN l=INT RPAREN f=field { Field.Lst { length=Some (snd l); mem=f; } }
  | ARRAY_UNLIMITED f=field { Field.Lst { length=None; mem=f; } }


var_list:
  | v=VAR vl=var_list { v :: vl }
  | { [] }


stmt_list:
  | s=stmt sl=stmt_list { s :: sl }
  | ST_VAR f=field IN sl=stmt_list { [ StVar (f, sl) ] }
  | ST_LET v=VAR EQ e=expr_full IN sl=stmt_list { [ StLet (snd v, e, sl) ] }
  | { [] }

stmt:
  | PLUS es=expr ei=expr? { StAdd (1, es, ei) }
  | MINUS es=expr ei=expr? { StAdd (-1, es, ei) }
  | DOT e=expr { StPut e }
  | COMMA e=expr { StGet e }
  | RSHIFT ep=expr ei=expr? { StShift (1, ep, ei) }
  | LSHIFT ep=expr ei=expr? { StShift (-1, ep, ei) }
  | EXCL e=expr LBRACKET sl=stmt_list RBRACKET { StWhile (e, sl) }
  | QUES e=expr LBRACKET sl_t=stmt_list RBRACKET LBRACKET sl_e=stmt_list RBRACKET {
      StIf (e, sl_t, Some sl_e)
    }
  | ASTER e=expr_appable { StExpand e }
  | ST_DIVE e=expr LBRACKET sl=stmt_list RBRACKET { StDive (e, sl) }

expr:
  | v=VAR { ExVar (snd v) }
  | i=INT { ExInt (snd i) }
  | c=CHAR { ExInt (int_of_char @@ snd c) }
  | s=STRING { ExStr (snd s) }
  | TRUE { ExBool true }
  | FALSE { ExBool false }
  | NIL { ExNil }
  | e=expr COLON v=VAR { ExSelMem (e, None, snd v) }
  | es=expr COLON LPAREN ei=expr_full RPAREN v=VAR { ExSelMem (es, Some ei, snd v) }
  | e=expr AT v=VAR { ExSelPtr (e, snd v) }
  | LBRACKET sl=stmt_list RBRACKET { ExBlock sl }
  | LPAREN e=expr_full RPAREN { e }

expr_full:
  | e=expr_appable { e }
  | FUN v=VAR vl=var_list ARROW e=expr_full {
      let e = List.fold_right (fun v e -> ExFun (snd v, e)) vl e in
      ExFun (snd v, e)
    } %prec prec_fun
  | IF ec=expr_full THEN et=expr_full ELSE ee=expr_full { ExIf (ec, et, ee) } %prec prec_if
  | LET v=VAR EQ e1=expr_full IN e2=expr_full { ExLet(snd v, e1, e2) } %prec prec_let
  | MATCH em=expr_full WITH BAR? NIL ARROW en=expr_full BAR vh=VAR CONS vt=VAR ARROW ec=expr_full END {
      ExMatch (em, en, snd vh, snd vt, ec)
    }
  | MINUS e=expr_full { ExMinus e }
  | e1=expr_full bop=bop_int e2=expr_full { ExBOpInt (e1, bop, e2) }
  | e1=expr_full EQ e2=expr_full { ExEqual (e1, e2) }
  | e1=expr_full CONS e2=expr_full { ExCons (e1, e2) }
  | e1=expr_full COMMA e2=expr_full { ExPair (e1, e2) }

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