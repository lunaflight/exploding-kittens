open! Core
open Protocol_lib

let all_mocked_outcomes =
  Action.Outcome.For_testing.all_mocked
    ~drew_safely:[ Powerless Cattermelon ]
    ~inserted_exploding_kitten:[ -10; -1; 0; 1; 10 ]
    ~saw_the_future:
      [ []
      ; [ Exploding_kitten ]
      ; [ Powerless Tacocat; Power Skip; Power See_the_future ]
      ]
;;

let print_alerts_of_outcomes outcomes ~alert_f =
  outcomes
  |> List.iter ~f:(fun outcome ->
    let alert = alert_f outcome in
    print_s [%message (outcome : Action.Outcome.t) (alert : string)])
;;

let%expect_test "outcome alerts for self look correct - full feedback is given" =
  print_alerts_of_outcomes all_mocked_outcomes ~alert_f:Action.Outcome.to_self_alert;
  [%expect
    {|
    ((outcome Defused) (alert "You defused an exploding kitten!"))
    ((outcome (Drew_safely (Powerless Cattermelon)))
     (alert "You drew a(n) Cattermelon."))
    ((outcome Exploded) (alert "You exploded!"))
    ((outcome (Inserted_exploding_kitten -10))
     (alert "You inserted an exploding kitten at position -10."))
    ((outcome (Inserted_exploding_kitten -1))
     (alert "You inserted an exploding kitten at position -1."))
    ((outcome (Inserted_exploding_kitten 0))
     (alert "You inserted an exploding kitten at position 0."))
    ((outcome (Inserted_exploding_kitten 1))
     (alert "You inserted an exploding kitten at position 1."))
    ((outcome (Inserted_exploding_kitten 10))
     (alert "You inserted an exploding kitten at position 10."))
    ((outcome (Saw_the_future ()))
     (alert "You did not see any cards as the deck is empty."))
    ((outcome (Saw_the_future (Exploding_kitten)))
     (alert "You saw 1 card at the top of the deck: Exploding Kitten"))
    ((outcome
      (Saw_the_future ((Powerless Tacocat) (Power Skip) (Power See_the_future))))
     (alert
      "You saw 3 cards at the top of the deck: Tacocat, Skip, See The Future"))
    ((outcome Skipped) (alert "You skipped your turn."))
    |}]
;;

let%expect_test "outcome alerts for others look correct - sensitive info is omitted" =
  print_alerts_of_outcomes
    all_mocked_outcomes
    ~alert_f:(Action.Outcome.to_others_alert ~name:"Alice");
  [%expect
    {|
    ((outcome Defused) (alert "Alice defused an exploding kitten!"))
    ((outcome (Drew_safely (Powerless Cattermelon)))
     (alert "Alice drew a card."))
    ((outcome Exploded) (alert "Alice exploded!"))
    ((outcome (Inserted_exploding_kitten -10))
     (alert "Alice inserted an exploding kitten somewhere."))
    ((outcome (Inserted_exploding_kitten -1))
     (alert "Alice inserted an exploding kitten somewhere."))
    ((outcome (Inserted_exploding_kitten 0))
     (alert "Alice inserted an exploding kitten somewhere."))
    ((outcome (Inserted_exploding_kitten 1))
     (alert "Alice inserted an exploding kitten somewhere."))
    ((outcome (Inserted_exploding_kitten 10))
     (alert "Alice inserted an exploding kitten somewhere."))
    ((outcome (Saw_the_future ()))
     (alert "Alice saw the future of 0 cards at the top of the deck."))
    ((outcome (Saw_the_future (Exploding_kitten)))
     (alert "Alice saw the future of 1 card at the top of the deck."))
    ((outcome
      (Saw_the_future ((Powerless Tacocat) (Power Skip) (Power See_the_future))))
     (alert "Alice saw the future of 3 cards at the top of the deck."))
    ((outcome Skipped) (alert "Alice skipped their turn."))
    |}]
;;
