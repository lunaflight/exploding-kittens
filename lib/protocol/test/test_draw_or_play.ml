open! Core
open Protocol_lib

let handle_and_print action ~hand ~deck =
  let player_under_test = Player_name.of_string "player_under_test" in
  let player_hands = Player_hands.For_testing.of_alist_exn [ player_under_test, hand ] in
  match
    Action.Draw_or_play.handle
      action
      ~player_hands
      ~player_name:player_under_test
      ~deck
      ~deterministically:true
  with
  | Error error -> print_s [%message (error : Error.t)]
  | Ok (outcome, player_hands, deck) ->
    print_s
      [%message (outcome : Outcome.t) (player_hands : Player_hands.t) (deck : Deck.t)]
;;

let%expect_test "draw from empty deck -> Error" =
  handle_and_print Draw ~hand:(Hand.of_cards []) ~deck:(Deck.For_testing.of_card_list []);
  [%expect {| (error "Attempting to draw from an empty deck") |}]
;;

let%expect_test "draw Exploding_kitten without defuse -> Exploded" =
  handle_and_print
    Draw
    ~hand:(Hand.of_cards [])
    ~deck:(Deck.For_testing.of_card_list [ Exploding_kitten ]);
  [%expect
    {|
    ((outcome Exploded)
     (player_hands ((player_under_test ((Exploding_kitten 1))))) (deck ()))
    |}]
;;

let%expect_test "draw Exploding_kitten with defuse -> Defused and consumed without \
                 holding the exploding kitten"
  =
  handle_and_print
    Draw
    ~hand:(Hand.of_cards [ Defuse ])
    ~deck:(Deck.For_testing.of_card_list [ Exploding_kitten ]);
  [%expect {| ((outcome Defused) (player_hands ((player_under_test ()))) (deck ())) |}]
;;

let%expect_test "draw non-Exploding_kitten -> drew successfully" =
  handle_and_print
    Draw
    ~hand:(Hand.of_cards [])
    ~deck:(Deck.For_testing.of_card_list [ Powerless Cattermelon; Exploding_kitten ]);
  [%expect
    {|
    ((outcome (Drew_safely (Powerless Cattermelon)))
     (player_hands ((player_under_test (((Powerless Cattermelon) 1)))))
     (deck (Exploding_kitten)))
    |}]
;;

let%expect_test "play Power without owning -> error" =
  handle_and_print
    (Play Skip)
    ~hand:(Hand.of_cards [])
    ~deck:(Deck.For_testing.of_card_list []);
  [%expect {| (error ("Card is not owned" (card (Power Skip)) (t ()))) |}]
;;

let%expect_test "play See_the_future with deck of 0 cards -> consumed and 0 seen" =
  handle_and_print
    (Play See_the_future)
    ~hand:(Hand.of_cards [ Power See_the_future ])
    ~deck:(Deck.For_testing.of_card_list []);
  [%expect
    {|
    ((outcome (Saw_the_future ())) (player_hands ((player_under_test ())))
     (deck ()))
    |}]
;;

let%expect_test "play See_the_future with deck of 1 card -> consumed and 1 seen" =
  handle_and_print
    (Play See_the_future)
    ~hand:(Hand.of_cards [ Power See_the_future ])
    ~deck:(Deck.For_testing.of_card_list [ Exploding_kitten ]);
  [%expect
    {|
    ((outcome (Saw_the_future (Exploding_kitten)))
     (player_hands ((player_under_test ()))) (deck (Exploding_kitten)))
    |}]
;;

let%expect_test "play See_the_future with deck of 4 cards -> consumed and 3 seen in order"
  =
  handle_and_print
    (Play See_the_future)
    ~hand:(Hand.of_cards [ Power See_the_future ])
    ~deck:
      (Deck.For_testing.of_card_list
         [ Powerless Beard_cat
         ; Powerless Cattermelon
         ; Powerless Rainbow_ralphing_cat
         ; Power Skip
         ]);
  [%expect
    {|
    ((outcome
      (Saw_the_future
       ((Powerless Beard_cat) (Powerless Cattermelon)
        (Powerless Rainbow_ralphing_cat))))
     (player_hands ((player_under_test ())))
     (deck
      ((Powerless Beard_cat) (Powerless Cattermelon)
       (Powerless Rainbow_ralphing_cat) (Power Skip))))
    |}]
;;

let%expect_test "play Skip -> consumed and no card drawn" =
  handle_and_print
    (Play Skip)
    ~hand:(Hand.of_cards [ Power Skip ])
    ~deck:(Deck.For_testing.of_card_list []);
  [%expect {| ((outcome Skipped) (player_hands ((player_under_test ()))) (deck ())) |}]
;;

let%expect_test "play Shuffle -> consumed and deck is shuffled" =
  handle_and_print
    (Play Shuffle)
    ~hand:(Hand.of_cards [ Power Shuffle ])
    ~deck:
      (Deck.For_testing.of_card_list
         [ Powerless Beard_cat
         ; Powerless Cattermelon
         ; Powerless Hairy_potato_cat
         ; Powerless Rainbow_ralphing_cat
         ; Powerless Tacocat
         ; Defuse
         ; Power Skip
         ; Power Shuffle
         ; Power See_the_future
         ; Exploding_kitten
         ]);
  [%expect
    {|
    ((outcome Shuffled) (player_hands ((player_under_test ())))
     (deck
      (Defuse (Powerless Cattermelon) (Power Skip) (Power See_the_future)
       (Powerless Tacocat) Exploding_kitten (Powerless Rainbow_ralphing_cat)
       (Powerless Hairy_potato_cat) (Power Shuffle) (Powerless Beard_cat))))
    |}]
;;
