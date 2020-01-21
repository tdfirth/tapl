open Core

(* TODO replace this with an actual eval... *)
let eval t = t

module Untyped : Meta.Language = struct
  type token = Parser.token

  type term = Term.term

  type 'a program = 'a list

  let parse = Parser.prog Lexer.read

  let eval = eval

  let string_of_term = Term.string_of_term

  let map = List.map

  let iter = List.iter
end
