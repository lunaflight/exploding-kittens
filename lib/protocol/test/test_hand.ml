open! Core
open Protocol_lib

let print_hand_of_cards cards =
  let hand = Hand.of_cards cards in
  print_endline [%string "%{hand#Hand}"]
;;

let%expect_test "String representation of 0 cards looks correct" =
  print_hand_of_cards [];
  [%expect {| |}]
;;

let%expect_test "String representation of 1 card looks correct" =
  print_hand_of_cards [ Powerless Beard_cat ];
  [%expect {| 1x Beard Cat |}]
;;

let%expect_test "String representation of many cards looks correct and sorted" =
  print_hand_of_cards
    [ Powerless Tacocat
    ; Powerless Tacocat
    ; Powerless Beard_cat
    ; Powerless Beard_cat
    ; Powerless Cattermelon
    ; Powerless Tacocat
    ];
  [%expect {| 2x Beard Cat, 1x Cattermelon, 3x Tacocat |}]
;;
