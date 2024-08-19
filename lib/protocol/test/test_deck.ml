open! Core
open Protocol_lib

let insert_and_print ~card ~deck ~position =
  let deck = Deck.insert deck ~card ~position in
  print_s [%message (deck : Deck.t)]
;;

let%expect_test "inserting into an empty deck at position 0 is ok" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [])
    ~position:0;
  [%expect {| (deck ((Powerless Tacocat))) |}]
;;

let%expect_test "inserting into an empty deck at positive position is ok" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [])
    ~position:3;
  [%expect {| (deck ((Powerless Tacocat))) |}]
;;

let%expect_test "inserting into an empty deck at negative position is ok" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [])
    ~position:(-3);
  [%expect {| (deck ((Powerless Tacocat))) |}]
;;

let%expect_test "inserting into a deck at position 0 makes it 1st" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [ Defuse; Defuse; Defuse ])
    ~position:0;
  [%expect {| (deck ((Powerless Tacocat) Defuse Defuse Defuse)) |}]
;;

let%expect_test "inserting into a deck at position 1 makes it 2nd" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [ Defuse; Defuse; Defuse ])
    ~position:1;
  [%expect {| (deck (Defuse (Powerless Tacocat) Defuse Defuse)) |}]
;;

let%expect_test "inserting into a deck at large positive position makes it last" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [ Defuse; Defuse; Defuse ])
    ~position:7;
  [%expect {| (deck (Defuse Defuse Defuse (Powerless Tacocat))) |}]
;;

let%expect_test "inserting into a deck at position -1 makes it last" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [ Defuse; Defuse; Defuse ])
    ~position:(-1);
  [%expect {| (deck (Defuse Defuse Defuse (Powerless Tacocat))) |}]
;;

let%expect_test "inserting into a deck at position -2 makes it 2nd last" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [ Defuse; Defuse; Defuse ])
    ~position:(-2);
  [%expect {| (deck (Defuse Defuse (Powerless Tacocat) Defuse)) |}]
;;

let%expect_test "inserting into a deck at large negative position makes it first" =
  insert_and_print
    ~card:(Powerless Tacocat)
    ~deck:(Deck.For_testing.of_card_list [ Defuse; Defuse; Defuse ])
    ~position:(-7);
  [%expect {| (deck ((Powerless Tacocat) Defuse Defuse Defuse)) |}]
;;
