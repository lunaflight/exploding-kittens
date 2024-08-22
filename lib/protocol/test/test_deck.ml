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

let add_exploding_kittens_and_print ~deck ~player_cnt =
  let deck = Deck.add_exploding_kittens deck ~player_cnt ~deterministically:true in
  print_s [%message (deck : Deck.t)]
;;

let%expect_test "add_exploding_kittens with 1 player -> 0 added and shuffled" =
  add_exploding_kittens_and_print
    ~deck:
      (Deck.For_testing.of_card_list
         [ Powerless Beard_cat
         ; Powerless Cattermelon
         ; Powerless Hairy_potato_cat
         ; Powerless Rainbow_ralphing_cat
         ; Powerless Tacocat
         ])
    ~player_cnt:1;
  [%expect
    {|
    (deck
     ((Powerless Rainbow_ralphing_cat) (Powerless Tacocat)
      (Powerless Hairy_potato_cat) (Powerless Cattermelon) (Powerless Beard_cat)))
    |}]
;;

let%expect_test "add_exploding_kittens with 2 players -> 1 added and shuffled" =
  add_exploding_kittens_and_print
    ~deck:
      (Deck.For_testing.of_card_list
         [ Powerless Beard_cat
         ; Powerless Cattermelon
         ; Powerless Hairy_potato_cat
         ; Powerless Rainbow_ralphing_cat
         ; Powerless Tacocat
         ])
    ~player_cnt:2;
  [%expect
    {|
    (deck
     ((Powerless Rainbow_ralphing_cat) (Powerless Beard_cat)
      (Powerless Cattermelon) Exploding_kitten (Powerless Tacocat)
      (Powerless Hairy_potato_cat)))
    |}]
;;

let%expect_test "add_exploding_kittens with 6 players -> 5 added and shuffled" =
  add_exploding_kittens_and_print
    ~deck:
      (Deck.For_testing.of_card_list
         [ Powerless Beard_cat
         ; Powerless Cattermelon
         ; Powerless Hairy_potato_cat
         ; Powerless Rainbow_ralphing_cat
         ; Powerless Tacocat
         ])
    ~player_cnt:6;
  [%expect
    {|
    (deck
     (Exploding_kitten (Powerless Cattermelon) Exploding_kitten Exploding_kitten
      (Powerless Tacocat) Exploding_kitten (Powerless Rainbow_ralphing_cat)
      (Powerless Hairy_potato_cat) Exploding_kitten (Powerless Beard_cat)))
    |}]
;;
