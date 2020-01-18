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

  let parse_file filename =
    let inx = In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let program = L.parse lexbuf in
    In_channel.close inx;
    program

  let eval_program program = L.map ~f:L.eval program

  let print_program program =
    L.iter ~f:(fun t -> L.string_of_term t |> Printf.printf "%s\n") program

  let interpret filename =
    let program = parse_file filename in
    eval_program program
end
