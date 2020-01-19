type term =
  | Variable of int * int
  | Abstraction of string * term
  | Application of term * term

(* let rec string_of_term = function
 *   | TmTrue -> "true"
 *   | TmFalse -> "false"
 *   | TmZero -> "0" *)
