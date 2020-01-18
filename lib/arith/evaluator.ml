open Core

module SmallStep = struct
  open Term

  type term = Term.term

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

module BigStep = struct
  open Term

  type term = Term.term

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
