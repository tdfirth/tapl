%{
open Term
%}

%token LAMBDA
%token <string> SYMBOL
%token LPAREN
%token RPAREN
%token DOT
%token SEMI
%token EOF

%start <Term.term list> prog
%%
prog:
  | t = term; SEMI; ts = prog         { t::ts }
  | EOF                               { [] }

term:
  | at = app_term                     { at }
  | LPAREN; t = term; RPAREN          { t }
  | LAMBDA; s = SYMBOL; DOT; t = term { Abstraction s t }

app_term:
  | abs = abs_term                    { abs }
  | app = app_term; abs = abs_term    { Application app abs }

/* TODO need to assign DeBruijn indices correctly */
abs_term:
  | LPAREN: t = term; RPAREN          { t }
  | s = SYMBOL                        { Variable 0 0 } 
