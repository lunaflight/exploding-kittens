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
      (Deck.Without_exploding_kittens.For_testing.of_card_list
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
      (Deck.Without_exploding_kittens.For_testing.of_card_list
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
      (Deck.Without_exploding_kittens.For_testing.of_card_list
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

let default_and_print ~player_cnt =
  let deck = Deck.Without_exploding_kittens.default ~player_cnt ~shuffled:false in
  print_s [%message (deck : Deck.Without_exploding_kittens.t Or_error.t)]
;;

let%expect_test "default with 1 player -> error" =
  default_and_print ~player_cnt:1;
  [%expect
    {| (deck (Error ("This deck is suited for 2 to 5 players." (player_cnt 1)))) |}]
;;

let%expect_test "default with 2 players -> contains 2 defuses and looks right" =
  default_and_print ~player_cnt:2;
  [%expect
    {|
    (deck
     (Ok
      (Defuse Defuse (Power See_the_future) (Power See_the_future)
       (Power See_the_future) (Power See_the_future) (Power See_the_future)
       (Power Skip) (Power Skip) (Power Skip) (Power Skip) (Power Shuffle)
       (Power Shuffle) (Power Shuffle) (Power Shuffle) (Powerless Beard_cat)
       (Powerless Beard_cat) (Powerless Beard_cat) (Powerless Beard_cat)
       (Powerless Cattermelon) (Powerless Cattermelon) (Powerless Cattermelon)
       (Powerless Cattermelon) (Powerless Hairy_potato_cat)
       (Powerless Hairy_potato_cat) (Powerless Hairy_potato_cat)
       (Powerless Hairy_potato_cat) (Powerless Rainbow_ralphing_cat)
       (Powerless Rainbow_ralphing_cat) (Powerless Rainbow_ralphing_cat)
       (Powerless Rainbow_ralphing_cat) (Powerless Tacocat) (Powerless Tacocat)
       (Powerless Tacocat) (Powerless Tacocat))))
    |}]
;;

let%expect_test "default with 5 players -> contains 1 defuse and looks right" =
  default_and_print ~player_cnt:5;
  [%expect
    {|
    (deck
     (Ok
      (Defuse (Power See_the_future) (Power See_the_future)
       (Power See_the_future) (Power See_the_future) (Power See_the_future)
       (Power Skip) (Power Skip) (Power Skip) (Power Skip) (Power Shuffle)
       (Power Shuffle) (Power Shuffle) (Power Shuffle) (Powerless Beard_cat)
       (Powerless Beard_cat) (Powerless Beard_cat) (Powerless Beard_cat)
       (Powerless Cattermelon) (Powerless Cattermelon) (Powerless Cattermelon)
       (Powerless Cattermelon) (Powerless Hairy_potato_cat)
       (Powerless Hairy_potato_cat) (Powerless Hairy_potato_cat)
       (Powerless Hairy_potato_cat) (Powerless Rainbow_ralphing_cat)
       (Powerless Rainbow_ralphing_cat) (Powerless Rainbow_ralphing_cat)
       (Powerless Rainbow_ralphing_cat) (Powerless Tacocat) (Powerless Tacocat)
       (Powerless Tacocat) (Powerless Tacocat))))
    |}]
;;

let%expect_test "default with 6 players -> error" =
  default_and_print ~player_cnt:6;
  [%expect
    {| (deck (Error ("This deck is suited for 2 to 5 players." (player_cnt 6)))) |}]
;;

let deal_and_print ~deck ~n =
  let deck = Deck.Without_exploding_kittens.deal deck ~n in
  match deck with
  | Error error -> print_s [%message (error : Error.t)]
  | Ok (hand, deck) ->
    print_s [%message (hand : Hand.t) (deck : Deck.Without_exploding_kittens.t)]
;;

let%expect_test "deal -1 cards -> error" =
  deal_and_print
    ~deck:(Deck.Without_exploding_kittens.For_testing.of_card_list [ Defuse ])
    ~n:(-1);
  [%expect
    {|
    (error
     ("Attempting to draw invalid number of cards" (n -1) ("List.length t" 1)))
    |}]
;;

let%expect_test "deal 0 cards from non-empty deck -> hand and deck look right" =
  deal_and_print
    ~deck:(Deck.Without_exploding_kittens.For_testing.of_card_list [ Defuse ])
    ~n:0;
  [%expect {| ((hand ()) (deck (Defuse))) |}]
;;

let%expect_test "deal 2 cards from non-empty deck -> hand and deck look right" =
  deal_and_print
    ~deck:
      (Deck.Without_exploding_kittens.For_testing.of_card_list
         [ Defuse
         ; Powerless Cattermelon
         ; Power Skip
         ; Power See_the_future
         ; Powerless Tacocat
         ])
    ~n:2;
  [%expect
    {|
    ((hand ((Defuse 1) ((Powerless Cattermelon) 1)))
     (deck ((Power Skip) (Power See_the_future) (Powerless Tacocat))))
    |}]
;;

let%expect_test "deal all cards from non-empty deck -> hand and deck look right" =
  deal_and_print
    ~deck:
      (Deck.Without_exploding_kittens.For_testing.of_card_list
         [ Defuse
         ; Powerless Cattermelon
         ; Power Skip
         ; Power See_the_future
         ; Powerless Tacocat
         ])
    ~n:5;
  [%expect
    {|
    ((hand
      ((Defuse 1) ((Power See_the_future) 1) ((Power Skip) 1)
       ((Powerless Cattermelon) 1) ((Powerless Tacocat) 1)))
     (deck ()))
    |}]
;;

let%expect_test "deal cards more than deck size -> error" =
  deal_and_print
    ~deck:
      (Deck.Without_exploding_kittens.For_testing.of_card_list
         [ Defuse
         ; Powerless Cattermelon
         ; Power Skip
         ; Power See_the_future
         ; Powerless Tacocat
         ])
    ~n:6;
  [%expect
    {|
    (error
     ("Attempting to draw invalid number of cards" (n 6) ("List.length t" 5)))
    |}]
;;

let%expect_test "deal 0 cards from empty deck -> ok" =
  deal_and_print ~deck:(Deck.Without_exploding_kittens.For_testing.of_card_list []) ~n:0;
  [%expect {| ((hand ()) (deck ())) |}]
;;

let%expect_test "deal 1 card from empty deck -> error" =
  deal_and_print ~deck:(Deck.Without_exploding_kittens.For_testing.of_card_list []) ~n:1;
  [%expect
    {|
    (error
     ("Attempting to draw invalid number of cards" (n 1) ("List.length t" 0)))
    |}]
;;
