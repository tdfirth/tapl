type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

type command = Eval of term

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
