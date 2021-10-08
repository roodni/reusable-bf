%{
module Lib = struct end (* おまじない *)
open Support.Error
open Syntax
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
%token <Support.Error.info> UNDER

%token <Support.Error.info> ST_ALLOC
%token <Support.Error.info> ST_LET
%token <Support.Error.info> ST_DIVE

%token <Support.Error.info> CELL PTR ARRAY ARRAY_UNLIMITED
%token <Support.Error.info> FUN
%token <Support.Error.info> LET IN
%token <Support.Error.info> IF THEN ELSE
%token <Support.Error.info> MATCH WITH
%token <Support.Error.info> MAIN
%token <Support.Error.info> MOD
%token <Support.Error.info> IMPORT AS

%token <int Support.Error.withinfo> INT
%token <char Support.Error.withinfo> CHAR
%token <string Support.Error.withinfo> STRING
%token <Syntax.Var.t Support.Error.withinfo> VAR
%token <Syntax.UVar.t Support.Error.withinfo> UVAR
%token <Support.Error.info> TRUE FALSE
%token <Support.Error.info> NIL

%nonassoc prec_fun prec_let prec_match
%nonassoc prec_if
%nonassoc BAR
%right COMMA
%left LSHIFT LEQ EQ
%right CONS
%left PLUS MINUS
%left ASTER SLASH MOD

%start program
%type <program> program

%%

program:
  | ts=toplevel_list m=main? EOF { (ts, m) }

toplevel_list:
  | t=toplevel ts=toplevel_list { t :: ts }
  | { [] }

toplevel:
  | i=LET lb=let_binding { withinfo i @@ TopLet lb }
  | i=IMPORT p=STRING { withinfo i @@ TopImport p.v }
  | i=IMPORT p=STRING AS u=UVAR { withinfo i @@ TopImportAs (p.v, u.v) }

main:
  | MAIN f=field IN sl=stmt_list { (f, sl) }
  | MAIN f=field LBRACKET sl=stmt_list RBRACKET { (f, sl) }

field:
  | LBRACE el=field_elm_list RBRACE { el }

field_elm_list:
  | e=field_elm el=field_elm_list { e :: el }
  | { [] }

field_elm:
  | v=VAR COLON ek=field_elm_mtype i2=SEMI { withinfo2 v.i i2 (v.v, ek) }

field_elm_mtype:
  | CELL { Field.Cell }
  | PTR { Field.Index }
  | ARRAY LPAREN l=INT RPAREN f=field { Field.Array { length=Some l.v; mem=f; } }
  | ARRAY_UNLIMITED f=field { Field.Array { length=None; mem=f; } }


stmt_list:
  | s=stmt sl=stmt_list { s :: sl }
  | i=ST_ALLOC f=field IN sl=stmt_list { [ withinfo i @@ StAlloc (f, sl) ] }
  | i=ST_LET lb=let_binding IN sl=stmt_list { [ withinfo i @@ StLet (lb, sl) ] }
  | { [] }

stmt:
  | i=PLUS es=expr ei=expr? { withinfo i @@ StAdd (1, es, ei) }
  | i=MINUS es=expr ei=expr? { withinfo i @@ StAdd (-1, es, ei) }
  | i=DOT e=expr { withinfo i @@ StPut e }
  | i=COMMA e=expr { withinfo i @@ StGet e }
  | i=RSHIFT ep=expr ei=expr? { withinfo i @@ StShift (1, ep, ei) }
  | i=LSHIFT ep=expr ei=expr? { withinfo i @@ StShift (-1, ep, ei) }
  | i=EXCL e=expr LBRACKET sl=stmt_list RBRACKET { withinfo i @@ StWhile (e, sl) }
  | i=QUES e=expr LBRACKET sl_t=stmt_list RBRACKET LBRACKET sl_e=stmt_list RBRACKET {
      withinfo i @@ StIf (e, sl_t, Some sl_e)
    }
  | i=ASTER e=expr_appable { withinfo i @@ StExpand e }
  | i=ST_ALLOC f=field LBRACKET sl=stmt_list RBRACKET { withinfo i @@ StAlloc (f, sl) }
  | i=ST_DIVE e=expr LBRACKET sl=stmt_list RBRACKET { withinfo i @@ StDive (e, sl) }

expr:
  | v=VAR { withinfo v.i @@ ExVar v.v }
  | uv=UVAR COLON v=VAR { withinfo2 uv.i v.i @@ ExModuleVar (uv.v, v.v) }
  | i=INT { withinfo i.i @@ ExInt i.v }
  | c=CHAR { withinfo c.i @@ ExInt (int_of_char c.v) }
  | s=STRING { withinfo s.i @@ ExStr s.v }
  | i=TRUE { withinfo i @@ ExBool true }
  | i=FALSE { withinfo i @@ ExBool false }
  | i=NIL { withinfo i ExNil }
  | e=expr COLON v=VAR { withinfo2 e.i v.i @@ ExSelMem (e, None, v.v) }
  | es=expr COLON LPAREN ei=expr_full RPAREN v=VAR {
      withinfo2 es.i v.i @@ ExSelMem (es, Some ei, v.v)
    }
  | e=expr AT v=VAR { withinfo2 e.i v.i @@ ExSelPtr (e, v.v) }
  | i1=LBRACKET sl=stmt_list i2=RBRACKET { withinfo2 i1 i2 @@ ExBlock sl }
  | i1=LPAREN e=expr_full i2=RPAREN { withinfo2 i1 i2 e.v }
  | i1=LPAREN e=expr_full SEMI l=expr_semi_list i2=RPAREN {
      withinfo2 i1 i2 @@ ExList (e :: l)
    }

expr_semi_list:
  | e=expr_full SEMI l=expr_semi_list { e :: l }
  | e=expr_full { [ e ] }
  | { [] }

expr_appable:
  | e=expr { e }
  | ef=expr_appable ea=expr { withinfo2 ef.i ea.i @@ ExApp (ef, ea) }

expr_full:
  | e=expr_appable { e }
  | i=FUN pl=pat_simple+ ARROW e=expr_full {
      List.fold_right
        (fun arg body -> withinfo2 i e.i @@ ExFun (arg, body))
        pl e
    } %prec prec_fun
  | i=IF ec=expr_full THEN et=expr_full ELSE ee=expr_full {
      withinfo2 i ee.i @@ ExIf (ec, et, ee)
    } %prec prec_if
  | i=LET lb=let_binding IN e=expr_full {
      withinfo2 i e.i @@ ExLet(lb, e)
    } %prec prec_let
  | i1=MATCH e=expr_full i2=WITH BAR? c=clauses {
      withinfo2 i1 i2 @@ ExMatch (e, c)
    }
  | i=MINUS e=expr_full { withinfo2 i e.i @@ ExMinus e }
  | e1=expr_full bop=bop_int e2=expr_full { withinfo2 e1.i e2.i @@ ExBOpInt (e1, bop, e2) }
  | e1=expr_full EQ e2=expr_full { withinfo2 e1.i e2.i @@ ExEqual (e1, e2) }
  | e1=expr_full CONS e2=expr_full { withinfo2 e1.i e2.i @@ ExCons (e1, e2) }
  | e1=expr_full COMMA e2=expr_full { withinfo2 e1.i e2.i @@ ExPair (e1, e2) }

%inline bop_int:
  | PLUS { BOpInt.Add }
  | MINUS { BOpInt.Sub }
  | ASTER { BOpInt.Mul }
  | SLASH { BOpInt.Div }
  | MOD { BOpInt.Mod }
  | LSHIFT { BOpInt.Lt }
  | LEQ { BOpInt.Leq }


pat:
  | p=pat_simple { p }
  | p1=pat CONS p2=pat { withinfo2 p1.i p2.i @@ PatCons (p1, p2) }
  | p1=pat COMMA p2=pat { withinfo2 p1.i p2.i @@ PatPair (p1, p2) }

pat_simple:
  | v=VAR { withinfo v.i @@ PatVar v.v }
  | i=UNDER { withinfo i PatWild }
  | i=NIL { withinfo i PatNil }
  | i=INT { withinfo i.i @@ PatInt i.v }
  | i=TRUE { withinfo i @@ PatBool true }
  | i=FALSE { withinfo i @@ PatBool false }
  | i1=LPAREN p=pat i2=RPAREN { withinfo2 i1 i2 @@ p.v }

clauses:
  | p=pat ARROW e=expr_full { [ (p, e) ] } %prec prec_match
  | p=pat ARROW e=expr_full BAR c=clauses { (p, e) :: c }


let_binding:
  | p=pat EQ e=expr_full { (p, e) }
  | v=VAR pl=pat_simple+ EQ e=expr_full {
      let body = List.fold_right
        (fun arg body -> withinfo2 arg.i e.i @@ ExFun (arg, body))
        pl e
      in
      (withinfo v.i @@ PatVar v.v, body)
    }