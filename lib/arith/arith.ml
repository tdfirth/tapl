open Core
module SmallStep = Evaluator.SmallStep
module BigStep = Evaluator.BigStep

module MakeArith (E : Meta.Evaluator with type term = Term.term) :
  Meta.Language = struct
  type token = Parser.token

  type term = Term.term

  type 'a program = 'a list

  let parse = Parser.prog Lexer.read

  let eval = E.eval

  let string_of_term = Term.string_of_term

  let map = List.map

  let iter = List.iter
end
