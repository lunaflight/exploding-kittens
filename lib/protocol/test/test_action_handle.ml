open! Core
open Protocol_lib

let handle_and_print action ~hand ~deck =
  match Action.handle action ~hand ~deck with
  | Error error -> print_s [%message (error : Error.t)]
  | Ok (outcome, hand, deck) ->
    print_s [%message (outcome : Action.Outcome.t) (hand : Card.t list) (deck : Deck.t)]
;;

let%expect_test "draw from empty deck -> Error" =
  handle_and_print Action.Draw ~hand:[] ~deck:(Deck.For_testing.of_card_list []);
  [%expect {| (error "Attempting to draw from an empty deck") |}]
;;

let%expect_test "draw Exploding_kitten -> Exploded" =
  handle_and_print
    Action.Draw
    ~hand:[]
    ~deck:(Deck.For_testing.of_card_list [ Exploding_kitten ]);
  [%expect {| ((outcome Exploded) (hand (Exploding_kitten)) (deck ())) |}]
;;

let%expect_test "draw Powerless -> Drew_successfully" =
  handle_and_print
    Action.Draw
    ~hand:[]
    ~deck:(Deck.For_testing.of_card_list [ Powerless Cattermelon; Exploding_kitten ]);
  [%expect
    {|
    ((outcome (Drew_successfully (Powerless Cattermelon)))
     (hand ((Powerless Cattermelon))) (deck (Exploding_kitten)))
    |}]
;;
