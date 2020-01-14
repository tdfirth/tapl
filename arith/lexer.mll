{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read =
  parse
  | '0'      { ZERO }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | ';'      { SEMI }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "iszero" { ISZERO }
  | "succ"   { SUCC }
  | "pred"   { PRED }
  | eof      { EOF }
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
