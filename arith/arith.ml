open Core

module type Evaluator = sig
  type t

  val eval : t -> t
end

module SmallStep : Evaluator with type t = Term.term = struct
  open Term

  type t = Term.term

  exception NoRuleApplies

  let rec eval t =
    let rec eval' = function
      | TmIf (TmTrue, t, _) -> t
      | TmIf (TmFalse, _, e) -> e
      | TmIf (i, t, e) -> TmIf (eval' i, t, e)
      | TmSucc s -> TmSucc (eval' s)
      | TmPred TmZero -> TmZero
      | TmPred (TmSucc s) when is_numeric s -> s
      | TmPred t -> TmPred (eval' t)
      | TmIsZero TmZero -> TmTrue
      | TmIsZero (TmSucc s) when is_numeric s -> TmFalse
      | TmIsZero z -> TmIsZero (eval' z)
      | _ -> raise NoRuleApplies
    in
    try
      let t' = eval' t in
      eval t'
    with NoRuleApplies -> t
end

module BigStep : Evaluator with type t = Term.term = struct
  open Term

  type t = Term.term

  let rec eval = function
    | TmIf (TmTrue, t, _) -> eval t
    | TmIf (TmFalse, _, e) -> eval e
    | TmIf (i, t, e) -> TmIf (eval i, t, e) |> eval
    | TmSucc s -> TmSucc (eval s)
    | TmPred TmZero -> TmZero
    | TmPred (TmSucc s) when is_numeric s -> eval s
    | TmPred t -> TmPred (eval t)
    | TmIsZero TmZero -> TmTrue
    | TmIsZero (TmSucc s) when is_numeric s -> TmFalse
    | TmIsZero z -> TmIsZero (eval z) |> eval
    | TmZero -> TmZero
    | TmFalse -> TmFalse
    | TmTrue -> TmTrue
end

module type Interpreter = sig
  val parse : string -> Term.command list

  val print_source : Term.command list -> unit

  val interpret : Term.command list -> Term.command list
end

module MakeInterpreter (E : Evaluator with type t = Term.term) : Interpreter =
struct
  open Lexer
  open Lexing

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

  let parse filename =
    let inx = In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let program = parse_with_error lexbuf in
    In_channel.close inx;
    program

  let print_source prog =
    List.iter prog ~f:(fun c -> Term.string_of_command c |> Printf.printf "%s")

  let interpret prog =
    let interpret_command = function Term.Eval t -> Term.Eval (E.eval t) in
    List.map prog ~f:interpret_command
end

exception InvalidEvaluator

let command =
  Command.basic ~summary:"Run and print Arith files."
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type)
      and evaluator =
        flag "-e"
          (optional_with_default "small" string)
          ~doc:"string  The evaluator to use."
      and print = flag "-p" no_arg ~doc:" Print the source code." in
      fun () ->
        let m =
          match evaluator with
          | "small" -> (module MakeInterpreter (SmallStep) : Interpreter)
          | "big" -> (module MakeInterpreter (BigStep) : Interpreter)
          | _ -> raise InvalidEvaluator
        in
        let module I = (val m) in
        let prog = I.parse filename in
        if print then I.print_source prog;
        I.interpret prog |> I.print_source)

let () = Command.run ~version:"1.0" command
