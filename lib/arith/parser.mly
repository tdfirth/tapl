%{
open Term
%}

%token ZERO
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LPAREN
%token RPAREN
%token SEMI
%token EOF

%start <Term.term list> prog
%%
prog:
  | t = term; SEMI; ts = prog { t::ts }
  | EOF { [] }

term:
  | SUCC; t = term { TmSucc(t) }
  | PRED; t = term { TmPred(t) }
  | ZERO { TmZero }
  | TRUE { TmTrue }
  | FALSE { TmFalse }
  | LPAREN; t = term; RPAREN { t }
  | ISZERO; t = term { TmIsZero(t) }
  | IF; i = term; THEN; t = term; ELSE; e = term { TmIf(i, t, e) }
