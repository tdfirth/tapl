open Core

let%expect_test "Test insert and get." =
  let module M = Meta.MakeEnv (String) in
  let env = M.empty () in
  let updated = M.insert env "foo" "bar" in
  Printf.printf "%s" (match M.get updated "foo" with None -> "" | Some s -> s);
  [%expect {|
      bar
  |}]
