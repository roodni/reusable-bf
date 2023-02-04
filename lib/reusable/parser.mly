%{
open Support.Pervasive
open Support.Info
open Syntax
%}

%token <Support.Info.info> EOF
%token <Support.Info.info> PLUS MINUS
%token <Support.Info.info> DOT COMMA
%token <Support.Info.info> RSHIFT LSHIFT  // > <
%token <Support.Info.info> LBRACKET RBRACKET
%token <Support.Info.info> EXCL QUES
%token <Support.Info.info> LBRACE RBRACE
%token <Support.Info.info> COLON
%token <Support.Info.info> AT
%token <Support.Info.info> ATAT // @@
%token <Support.Info.info> SEMI
%token <Support.Info.info> LPAREN RPAREN
%token <Support.Info.info> EQ NEQ LEQ GEQ
%token <Support.Info.info> ASTER
%token <Support.Info.info> ASTER2 // **
%token <Support.Info.info> SLASH
%token <Support.Info.info> ARROW  // ->
%token <Support.Info.info> UNDER
%token <Support.Info.info> BAR  // |
%token <Support.Info.info> BARBAR // ||
%token <Support.Info.info> ANDAND // &&
%token <Support.Info.info> MOD  // %


%token <Support.Info.info> ST  // $
%token <Support.Info.info> ST_ALLOC ST_BUILD
%token <Support.Info.info> ST_DIVE
%token <Support.Info.info> ST_ILOOP

%token <Support.Info.info> CELL INDEX ARRAY
%token <Support.Info.info> FUN
%token <Support.Info.info> LET IN
%token <Support.Info.info> IF THEN ELSE
%token <Support.Info.info> MATCH WITH

%token <Support.Info.info> IMPORT
%token <Support.Info.info> MODULE
%token <Support.Info.info> OPEN
%token <Support.Info.info> INCLUDE
%token <Support.Info.info> STRUCT
%token <Support.Info.info> END
%token <Support.Info.info> CODEGEN

%token <int Support.Info.withinfo> INT
%token <char Support.Info.withinfo> CHAR
%token <string Support.Info.withinfo> STRING
%token <Syntax.Var.t Support.Info.withinfo> VAR
%token <Syntax.UVar.t Support.Info.withinfo> UVAR
%token <Support.Info.info> TRUE FALSE

%nonassoc prec_stmts
%nonassoc prec_fun prec_let prec_match
%nonassoc prec_if
%nonassoc BAR
%right COMMA
%right BARBAR
%right ANDAND
%left LSHIFT RSHIFT LEQ GEQ EQ NEQ
%right ATAT
%right DOT
%left PLUS MINUS
%left ASTER SLASH MOD

%start program
%type <program> program

%%

program:
  | ts=toplevel_list EOF { llist ts }

toplevel_list:
  | t=toplevel ts=toplevel_list { t :: ts }
  | i=CODEGEN ST sl=stmts { [withinfo i @@ TopCodegen sl] }
  | { [] }

toplevel:
  | i=LET lb=let_binding { withinfo i @@ TopLet lb }
  | i=CODEGEN LBRACKET sl=stmts RBRACKET { withinfo i @@ TopCodegen sl }
  | i=OPEN m=mod_expr { withinfo i @@ TopOpen m }
  | i=INCLUDE m=mod_expr { withinfo i @@ TopInclude m }
  | i=MODULE v=UVAR EQ m=mod_expr { withinfo i @@ TopModule (v.v, m) }

mod_expr:
  | i=IMPORT s=STRING { withinfo2 i s.i @@ ModImport s.v }
  | i1=STRUCT ts=toplevel_list i2=END { withinfo2 i1 i2 @@ ModStruct (llist ts) }
  | l=separated_nonempty_list(COLON, UVAR) {
      match l with
      | [] -> assert false
      | hd :: _ ->
          let last = List.rev l |> List.hd in
          withinfo2 hd.i last.i @@ ModVar (llist l |> LList.map clearinfo)
    }

field:
  | i1=LBRACE el=field_elm_list i2=RBRACE { withinfo2 i1 i2 @@ llist el }

field_elm_list:
  | e=field_elm SEMI el=field_elm_list { e :: el }
  | e=field_elm { [e] }
  | { [] }

field_elm:
  | v=VAR COLON mt=mtype_expr { withinfo2 v.i mt.i (v.v, mt.v) }
  | v=VAR { withinfo v.i (v.v, MtyExCell) }

mtype_expr:
  | i=CELL { withinfo i MtyExCell }
  | i=INDEX { withinfo i MtyExIndex }
  | i=ARRAY LPAREN e=expr_full RPAREN f=field {
      withinfo2 i f.i @@ MtyExArray { length=Some e; mem=f.v; }
    }
  | i=ARRAY LPAREN UNDER RPAREN f=field {
      withinfo2 i f.i @@ MtyExArray { length=None; mem=f.v; }
    }

stmts:
  | s=stmt sl=stmts { lcons s sl }
  | i=ST_ALLOC f=field sl=stmts { llist [ withinfo i @@ StAlloc (f.v, sl) ] }
  | i=ST_BUILD f=field sl=stmts { llist [ withinfo i @@ StBuild (f.v, sl) ] }
  | i=ASTER2 e=expr_full { llist [ withinfo i @@ StExpand e ] } %prec prec_stmts
  | { lnil } %prec prec_stmts

stmt:
  | i=PLUS es=expr ei=expr? { withinfo i @@ StAdd (1, es, ei) }
  | i=MINUS es=expr ei=expr? { withinfo i @@ StAdd (-1, es, ei) }
  | i=DOT e=expr { withinfo i @@ StPut e }
  | i=COMMA e=expr { withinfo i @@ StGet e }
  | i=RSHIFT ep=expr { withinfo i @@ StShift (1, ep, None) }
  | i=LSHIFT ep=expr { withinfo i @@ StShift (-1, ep, None) }
  | i=EXCL e=expr LBRACKET sl=stmts RBRACKET { withinfo i @@ StWhile (e, sl) }
  | i=QUES e=expr LBRACKET sl_t=stmts RBRACKET LBRACKET sl_e=stmts RBRACKET {
      withinfo i @@ StIf (e, sl_t, Some sl_e)
    }
  | i=ST_ILOOP e=expr LBRACKET sl=stmts RBRACKET { withinfo i @@ StILoop (e, sl) }
  | i=ASTER e=expr_appable { withinfo i @@ StExpand e }
  | i=ST_DIVE e=expr LBRACKET sl=stmts RBRACKET { withinfo i @@ StDive (e, sl) }

expr:
  | l=uvar_list v=VAR {
      match l with
      | [] -> withinfo v.i @@ ExVar (lnil, v.v)
      | hd :: _ ->
          withinfo2 hd.i v.i @@
            ExVar (llist l |> LList.map clearinfo, v.v)
    }
  | i=INT { withinfo i.i @@ ExInt i.v }
  | c=CHAR { withinfo c.i @@ ExInt (int_of_char c.v) }
  | s=STRING { withinfo s.i @@ ExStr s.v }
  | i=TRUE { withinfo i @@ ExBool true }
  | i=FALSE { withinfo i @@ ExBool false }
  | e=expr COLON v=VAR { withinfo2 e.i v.i @@ ExSelMem (e, None, v.v) }
  | es=expr COLON LPAREN ei=expr_full RPAREN v=VAR {
      withinfo2 es.i v.i @@ ExSelMem (es, Some ei, v.v)
    }
  | e=expr AT v=VAR { withinfo2 e.i v.i @@ ExSelIdx (e, v.v) }
  | i1=LBRACKET sl=stmts i2=RBRACKET { withinfo2 i1 i2 @@ ExBlock sl }
  | i1=LPAREN e=expr_full i2=RPAREN { withinfo2 i1 i2 e.v }
  | i1=LPAREN e=expr_full SEMI l=expr_semi_list i2=RPAREN {
      withinfo2 i1 i2 @@ ExList (llist (e :: l))
    }
  | i1=LPAREN i2=RPAREN { withinfo2 i1 i2 @@ ExList lnil }

uvar_list:
  | uv=UVAR COLON l=uvar_list { uv :: l }
  | { [] }

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
      List.rev pl
      |> List.fold_left
        (fun body arg -> withinfo2 i e.i @@ ExFun (arg, body))
        e
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
  | i=ST sl=stmts { withinfo i @@ ExBlock sl }
  | e1=expr_full ANDAND e2=expr_full { withinfo2 e1.i e2.i @@ ExAnd (e1, e2) }
  | e1=expr_full BARBAR e2=expr_full { withinfo2 e1.i e2.i @@ ExOr (e1, e2) }
  | i=MINUS e=expr_full { withinfo2 i e.i @@ ExMinus e }
  | e1=expr_full bop=bop_int e2=expr_full { withinfo2 e1.i e2.i @@ ExBOpInt (e1, bop, e2) }
  | e1=expr_full EQ e2=expr_full { withinfo2 e1.i e2.i @@ ExEqual (`Eq, e1, e2) }
  | e1=expr_full NEQ e2=expr_full { withinfo2 e1.i e2.i @@ ExEqual (`Neq, e1, e2) }
  | e1=expr_full ATAT e2=expr_full { withinfo2 e1.i e2.i @@ ExApp (e1, e2) }
  | e1=expr_full DOT e2=expr_full { withinfo2 e1.i e2.i @@ ExCons (e1, e2) }
  | e1=expr_full COMMA e2=expr_full { withinfo2 e1.i e2.i @@ ExPair (e1, e2) }

%inline bop_int:
  | PLUS { BOp.Add }
  | MINUS { BOp.Sub }
  | ASTER { BOp.Mul }
  | SLASH { BOp.Div }
  | MOD { BOp.Mod }
  | LSHIFT { BOp.Lt }
  | RSHIFT { BOp.Gt }
  | LEQ { BOp.Leq }
  | GEQ { BOp.Geq }

pat_full:
  | p=pat_simple { p }
  | p1=pat_full DOT p2=pat_full { withinfo2 p1.i p2.i @@ PatCons (p1, p2) }
  | p1=pat_full COMMA p2=pat_full { withinfo2 p1.i p2.i @@ PatPair (p1, p2) }

pat_simple:
  | v=VAR { withinfo v.i @@ PatVar v.v }
  | i=UNDER { withinfo i PatWild }
  | i=INT { withinfo i.i @@ PatInt i.v }
  | i=TRUE { withinfo i @@ PatBool true }
  | i=FALSE { withinfo i @@ PatBool false }
  | i1=LPAREN p=pat_full i2=RPAREN { withinfo2 i1 i2 @@ p.v }
  | i1=LPAREN i2=RPAREN { withinfo2 i1 i2 PatNil }

clauses:
  | p=pat_full ARROW e=expr_full { llist [(p, e)] } %prec prec_match
  | p=pat_full ARROW e=expr_full BAR c=clauses { lcons (p, e) c }


let_binding:
  | p=pat_full EQ e=expr_full { (p, e) }
  | v=VAR pl=pat_simple+ EQ e=expr_full {
      let body =
        List.rev pl
        |> List.fold_left
          (fun body arg -> withinfo2 arg.i e.i @@ ExFun (arg, body))
          e
      in
      (withinfo v.i @@ PatVar v.v, body)
    }