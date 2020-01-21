open Lexing

module type Evaluator = sig
  type term

  val eval : term -> term
end

module type Language = sig
  type token

  type term

  type 'a program

  val parse : lexbuf -> term program

  val eval : term -> term

  val string_of_term : term -> string

  val map : 'a program -> f:('a -> 'b) -> 'b program

  val iter : 'a program -> f:('a -> unit) -> unit
end

module type Interpreter = sig
  type program

  val interpret : string -> program

  val print_program : program -> unit
end

module MakeInterpreter (L : Language) : Interpreter = struct
  open Core

  type program = L.term L.program

  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  let print_lexeme outx lexbuf =
    let lexeme = Lexing.lexeme lexbuf in
    fprintf outx "%s" lexeme

  let parse_file filename =
    let inx = In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    try
      let program = L.parse lexbuf in
      In_channel.close inx;
      program
    with _ ->
      fprintf stderr "%a Error encountered\n" print_position lexbuf;
      fprintf stderr "%a Error encountered\n" print_lexeme lexbuf;
      exit 1

  let eval_program program = L.map ~f:L.eval program

  let print_program program =
    L.iter ~f:(fun t -> L.string_of_term t |> Printf.printf "%s\n") program

  let interpret filename =
    let program = parse_file filename in
    eval_program program
end
