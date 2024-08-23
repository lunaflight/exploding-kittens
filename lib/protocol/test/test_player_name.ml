open! Core
open Protocol_lib

let%expect_test "player name equality is case insensitive" =
  let is_equal =
    Player_name.equal (Player_name.of_string "a") (Player_name.of_string "A")
  in
  print_s [%message (is_equal : bool)];
  [%expect {| (is_equal true) |}]
;;
