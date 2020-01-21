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
let symbol = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read =
  parse
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | ';'      { SEMI }
  | '.'      { DOT }
  | "lambda" { LAMBDA }
  | symbol   { SYMBOL (Lexing.lexeme lexbuf) }
  | eof      { EOF }
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
