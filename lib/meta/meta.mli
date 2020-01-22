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

  val read : string -> origin:string -> program

  val eval : program -> program

  val print : program -> unit
end

module MakeInterpreter (L : Language) : Interpreter
