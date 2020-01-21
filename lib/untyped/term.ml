type term =
  | Variable of int * int
  | Abstraction of string * term
  | Application of term * term

let rec string_of_term = function
  | Variable (x, y) -> Printf.sprintf "Var( %d, %d )" x y
  | Abstraction (s, t) ->
      Printf.sprintf "Abstraction ( %s, %s )" s (string_of_term t)
  | Application (x, y) ->
      Printf.sprintf "Application ( %s, %s )" (string_of_term x)
        (string_of_term y)
