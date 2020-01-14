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

%start <Term.command list> prog
%%
prog:
  | c = command; SEMI; cs = prog { c::cs }
  | EOF { [] }

command:
  | t = term { Eval(t) }

term:
  | SUCC; t = term { TmSucc(t) }
  | PRED; t = term { TmPred(t) }
  | ZERO { TmZero }
  | TRUE { TmTrue }
  | FALSE { TmFalse }
  | LPAREN; t = term; RPAREN { t }
  | ISZERO; t = term { TmIsZero(t) }
  | IF; i = term; THEN; t = term; ELSE; e = term { TmIf(i, t, e) }
