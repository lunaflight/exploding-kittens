open! Core
open Protocol_lib

let handle_and_print action ~hand ~deck =
  match Action.handle action ~hand ~deck with
  | Error error -> print_s [%message (error : Error.t)]
  | Ok (outcome, hand, deck) ->
    print_s [%message (outcome : Action.Outcome.t) (hand : Hand.t) (deck : Deck.t)]
;;

let%expect_test "draw from empty deck -> Error" =
  handle_and_print Draw ~hand:(Hand.of_cards []) ~deck:(Deck.For_testing.of_card_list []);
  [%expect {| (error "Attempting to draw from an empty deck") |}]
;;

let%expect_test "draw Exploding_kitten -> Exploded" =
  handle_and_print
    Draw
    ~hand:(Hand.of_cards [])
    ~deck:(Deck.For_testing.of_card_list [ Exploding_kitten ]);
  [%expect {| ((outcome Exploded) (hand ((Exploding_kitten 1))) (deck ())) |}]
;;

let%expect_test "draw non-Exploding_kitten -> drew successfully" =
  handle_and_print
    Draw
    ~hand:(Hand.of_cards [])
    ~deck:(Deck.For_testing.of_card_list [ Powerless Cattermelon; Exploding_kitten ]);
  [%expect
    {|
    ((outcome (Drew (Powerless Cattermelon)))
     (hand (((Powerless Cattermelon) 1))) (deck (Exploding_kitten)))
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
  [%expect {| ((outcome (Saw_the_future ())) (hand ()) (deck ())) |}]
;;

let%expect_test "play See_the_future with deck of 1 card -> consumed and 1 seen" =
  handle_and_print
    (Play See_the_future)
    ~hand:(Hand.of_cards [ Power See_the_future ])
    ~deck:(Deck.For_testing.of_card_list [ Exploding_kitten ]);
  [%expect
    {|
    ((outcome (Saw_the_future (Exploding_kitten))) (hand ())
     (deck (Exploding_kitten)))
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
     (hand ())
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
  [%expect {| ((outcome Skipped) (hand ()) (deck ())) |}]
;;
