module S = Meta.MakeInterpreter (Arith.MakeArith (Arith.SmallStep))
module B = Meta.MakeInterpreter (Arith.MakeArith (Arith.BigStep))

let fixture =
  {|
  true;
  if false then true else false;
  0;
  succ (pred 0);
  iszero (pred (succ (succ 0)));
|}

let%expect_test "Small step arith evaluator." =
  Testing.eval (module S) fixture;
  [%expect {|
    true
    false
    0
    succ 0
    false |}]

let%expect_test "Big step arith evaluator." =
  Testing.eval (module B) fixture;
  [%expect {|
    true
    false
    0
    succ 0
    false |}]
