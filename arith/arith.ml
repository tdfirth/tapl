open Core
open Lexer
open Lexing
open Term

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
      Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
      exit (-1)
  | Parser.Error ->
      Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let rec string_of_term = function
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmZero -> "0"
  | TmSucc s -> "succ " ^ string_of_term s
  | TmPred s -> "pred " ^ string_of_term s
  | TmIsZero s -> "iszero " ^ string_of_term s
  | TmIf (i, t, e) ->
      "if " ^ string_of_term i ^ " then " ^ string_of_term t ^ " else "
      ^ string_of_term e

let string_of_command = function Eval t -> string_of_term t ^ ";\n"

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let program = parse_with_error lexbuf in
  List.iter program ~f:(fun c -> string_of_command c |> Printf.printf "%s");
  In_channel.close inx

let () =
  Command.basic_spec ~summary:"Parse and display Arith"
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command.run
