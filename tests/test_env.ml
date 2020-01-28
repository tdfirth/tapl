open Core
module M = Meta.MakeEnv (String)

let%expect_test "Test insert and get." =
  let env = M.empty () in
  let updated = M.insert env "a" 1 in
  let updated = M.insert updated "b" 2 in
  let updated = M.insert updated "c" 3 in
  let get k = match M.get updated k with None -> 0 | Some s -> s in
  Printf.printf "%d %d %d %d" (get "a") (get "b") (get "c") (get "foo");
  [%expect {|
      1 2 3 0
  |}]

let%test "Test count." =
  let env = M.empty () in
  let updated = M.insert env "a" 1 in
  let updated = M.insert updated "b" 2 in
  let updated = M.insert updated "c" 3 in
  let updated = M.insert updated "d" 4 in
  let updated = M.insert updated "e" 5 in
  let updated = M.insert updated "f" 6 in
  let updated = M.insert updated "g" 7 in
  let updated = M.insert updated "h" 8 in
  M.count updated = 8

let%test "Test empty depth" =
  let env = M.empty () in
  M.depth env = 0

let%test "Test depth of one" =
  let env = M.empty () in
  M.depth (M.insert env "a" 1) = 1

(*
 *           d:4
 *     b:2         f:6
 *  a:1   c:3   e:5   g:7
 *)
let%test "Test depth balanced" =
  let env = M.empty () in
  let updated = M.insert env "d" 4 in
  let updated = M.insert updated "b" 2 in
  let updated = M.insert updated "c" 3 in
  let updated = M.insert updated "a" 1 in
  let updated = M.insert updated "f" 6 in
  let updated = M.insert updated "e" 5 in
  let updated = M.insert updated "g" 7 in
  M.depth updated = 3 && M.count updated = 7

let%test "Unbalanced right" =
  let env = M.empty () in
  let updated = M.insert env "a" 1 in
  let updated = M.insert updated "b" 2 in
  let updated = M.insert updated "c" 3 in
  let updated = M.insert updated "d" 4 in
  let updated = M.insert updated "e" 5 in
  let updated = M.insert updated "f" 6 in
  let updated = M.insert updated "g" 7 in
  let updated = M.insert updated "h" 8 in
  M.depth updated = 8

let%test "Unbalanced left" =
  let env = M.empty () in
  let updated = M.insert env "h" 8 in
  let updated = M.insert updated "g" 7 in
  let updated = M.insert updated "f" 6 in
  let updated = M.insert updated "e" 5 in
  let updated = M.insert updated "d" 4 in
  let updated = M.insert updated "c" 3 in
  let updated = M.insert updated "b" 2 in
  let updated = M.insert updated "a" 1 in
  M.depth updated = 8
