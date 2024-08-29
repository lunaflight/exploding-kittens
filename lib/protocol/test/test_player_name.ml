open! Core
open Protocol_lib

let%expect_test "player name equality is case insensitive" =
  let is_equal =
    Player_name.equal (Player_name.of_string_exn "a") (Player_name.of_string_exn "A")
  in
  print_s [%message (is_equal : bool)];
  [%expect {| (is_equal true) |}]
;;

let player_name_of_string_and_print string =
  let player_name = Player_name.of_string_or_error string in
  print_s [%message (player_name : Player_name.t Or_error.t)]
;;

let%expect_test "empty player name is invalid" =
  player_name_of_string_and_print "";
  [%expect {| (player_name (Error ("Invalid player name" (string "")))) |}]
;;

let%expect_test "player name with @ is invalid" =
  player_name_of_string_and_print "a@a";
  [%expect {| (player_name (Error ("Invalid player name" (string a@a)))) |}]
;;

let%expect_test "valid player name -> Ok returned" =
  player_name_of_string_and_print "aa";
  [%expect {| (player_name (Ok aa)) |}]
;;
