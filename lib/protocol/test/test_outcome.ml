open! Core
open Protocol_lib

let all_mocked_outcomes =
  Outcome.For_testing.all_mocked
    ~drew_safely:[ Powerless Cattermelon ]
    ~inserted_exploding_kitten:[ -10; -1; 0; 1; 10 ]
    ~failed_to_steal_via_triple:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
    ~favored:[ Player_name.of_string_exn "Somebody" ]
    ~received_card_from:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody" ]
    ~saw_the_future:
      [ []
      ; [ Exploding_kitten ]
      ; [ Powerless Tacocat
        ; Power (Targetless Skip)
        ; Power (Targetless See_the_future)
        ]
      ]
    ~stole_via_triple:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
    ~stole_randomly_via_double:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
;;

let print_alerts_of_outcomes outcomes ~alert_f =
  outcomes
  |> List.iter ~f:(fun outcome ->
    let alert = alert_f outcome in
    print_s [%message (outcome : Outcome.t) (alert : string)])
;;

let%expect_test "outcome alerts for self look correct - full feedback is given" =
  print_alerts_of_outcomes all_mocked_outcomes ~alert_f:Outcome.to_self_alert;
  [%expect
    {|
    ((outcome Attacked) (alert "You passed your turn by attacking."))
    ((outcome Defused) (alert "You defused an exploding kitten!"))
    ((outcome (Drew_safely (Powerless Cattermelon)))
     (alert "You drew a(n) Cattermelon."))
    ((outcome Exploded) (alert "You exploded!"))
    ((outcome
      (Failed_to_steal_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "You played a(n) Cattermelon triple and failed to steal a(n) Defuse from Somebody."))
    ((outcome (Favored Somebody)) (alert "You asked Somebody for a favor."))
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
    ((outcome (Received_card_from ((Powerless Cattermelon) Somebody)))
     (alert "You received a(n) Cattermelon from Somebody."))
    ((outcome (Saw_the_future ()))
     (alert "You did not see any cards as the deck is empty."))
    ((outcome (Saw_the_future (Exploding_kitten)))
     (alert "You saw 1 card at the top of the deck: Exploding Kitten."))
    ((outcome
      (Saw_the_future
       ((Powerless Tacocat) (Power (Targetless Skip))
        (Power (Targetless See_the_future)))))
     (alert
      "You saw 3 cards at the top of the deck: Tacocat, Skip, See The Future."))
    ((outcome Shuffled) (alert "You shuffled the deck."))
    ((outcome Skipped) (alert "You skipped your turn."))
    ((outcome
      (Stole_randomly_via_double ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "You played a(n) Cattermelon double and randomly stole a(n) Defuse from Somebody."))
    ((outcome (Stole_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "You played a(n) Cattermelon triple and stole a(n) Defuse from Somebody."))
    |}]
;;

let%expect_test "outcome alerts for spectators look correct - same as \
                 [to_self_alert] but with the name changed"
  =
  print_alerts_of_outcomes
    all_mocked_outcomes
    ~alert_f:
      (Outcome.to_uncensored_alert
         ~player_name:(Player_name.of_string_exn "Alice"));
  [%expect
    {|
    ((outcome Attacked) (alert "Alice passed their turn by attacking."))
    ((outcome Defused) (alert "Alice defused an exploding kitten!"))
    ((outcome (Drew_safely (Powerless Cattermelon)))
     (alert "Alice drew a(n) Cattermelon."))
    ((outcome Exploded) (alert "Alice exploded!"))
    ((outcome
      (Failed_to_steal_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "Alice played a(n) Cattermelon triple and failed to steal a(n) Defuse from Somebody."))
    ((outcome (Favored Somebody)) (alert "Alice asked Somebody for a favor."))
    ((outcome (Inserted_exploding_kitten -10))
     (alert "Alice inserted an exploding kitten at position -10."))
    ((outcome (Inserted_exploding_kitten -1))
     (alert "Alice inserted an exploding kitten at position -1."))
    ((outcome (Inserted_exploding_kitten 0))
     (alert "Alice inserted an exploding kitten at position 0."))
    ((outcome (Inserted_exploding_kitten 1))
     (alert "Alice inserted an exploding kitten at position 1."))
    ((outcome (Inserted_exploding_kitten 10))
     (alert "Alice inserted an exploding kitten at position 10."))
    ((outcome (Received_card_from ((Powerless Cattermelon) Somebody)))
     (alert "Alice received a(n) Cattermelon from Somebody."))
    ((outcome (Saw_the_future ()))
     (alert "Alice did not see any cards as the deck is empty."))
    ((outcome (Saw_the_future (Exploding_kitten)))
     (alert "Alice saw 1 card at the top of the deck: Exploding Kitten."))
    ((outcome
      (Saw_the_future
       ((Powerless Tacocat) (Power (Targetless Skip))
        (Power (Targetless See_the_future)))))
     (alert
      "Alice saw 3 cards at the top of the deck: Tacocat, Skip, See The Future."))
    ((outcome Shuffled) (alert "Alice shuffled the deck."))
    ((outcome Skipped) (alert "Alice skipped their turn."))
    ((outcome
      (Stole_randomly_via_double ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "Alice played a(n) Cattermelon double and randomly stole a(n) Defuse from Somebody."))
    ((outcome (Stole_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "Alice played a(n) Cattermelon triple and stole a(n) Defuse from Somebody."))
    |}]
;;

let%expect_test "outcome alerts for others look correct - sensitive info is \
                 omitted"
  =
  print_alerts_of_outcomes
    all_mocked_outcomes
    ~alert_f:
      (Outcome.to_censored_alert
         ~player_name:(Player_name.of_string_exn "Alice"));
  [%expect
    {|
    ((outcome Attacked) (alert "Alice passed their turn by attacking."))
    ((outcome Defused) (alert "Alice defused an exploding kitten!"))
    ((outcome (Drew_safely (Powerless Cattermelon)))
     (alert "Alice drew a card."))
    ((outcome Exploded) (alert "Alice exploded!"))
    ((outcome
      (Failed_to_steal_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "Alice played a(n) Cattermelon triple and failed to steal a(n) Defuse from Somebody."))
    ((outcome (Favored Somebody)) (alert "Alice asked Somebody for a favor."))
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
    ((outcome (Received_card_from ((Powerless Cattermelon) Somebody)))
     (alert "Alice received a card from Somebody."))
    ((outcome (Saw_the_future ()))
     (alert "Alice saw the future of 0 cards at the top of the deck."))
    ((outcome (Saw_the_future (Exploding_kitten)))
     (alert "Alice saw the future of 1 card at the top of the deck."))
    ((outcome
      (Saw_the_future
       ((Powerless Tacocat) (Power (Targetless Skip))
        (Power (Targetless See_the_future)))))
     (alert "Alice saw the future of 3 cards at the top of the deck."))
    ((outcome Shuffled) (alert "Alice shuffled the deck."))
    ((outcome Skipped) (alert "Alice skipped their turn."))
    ((outcome
      (Stole_randomly_via_double ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "Alice played a(n) Cattermelon double and randomly stole a card from Somebody."))
    ((outcome (Stole_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (alert
      "Alice played a(n) Cattermelon triple and stole a(n) Defuse from Somebody."))
    |}]
;;

let print_specialised_alerts_of_outcomes outcomes ~player_name =
  outcomes
  |> List.iter ~f:(fun outcome ->
    match Outcome.to_specialised_alert outcome ~player_name with
    | None -> print_s [%message (outcome : Outcome.t) "<no specialised alert>"]
    | Some (player, alert) ->
      print_s
        [%message
          (outcome : Outcome.t) (player : Player_name.t) (alert : string)])
;;

let%expect_test "specialised outcome alerts and recipients look correct - full \
                 feedback is given accordingly"
  =
  print_specialised_alerts_of_outcomes
    all_mocked_outcomes
    ~player_name:(Player_name.of_string_exn "Alice");
  [%expect
    {|
    ((outcome Attacked) "<no specialised alert>")
    ((outcome Defused) "<no specialised alert>")
    ((outcome (Drew_safely (Powerless Cattermelon))) "<no specialised alert>")
    ((outcome Exploded) "<no specialised alert>")
    ((outcome
      (Failed_to_steal_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (player Somebody)
     (alert
      "Alice played a(n) Cattermelon triple and failed to steal a(n) Defuse from you."))
    ((outcome (Favored Somebody)) (player Somebody)
     (alert "Alice asked you for a favor."))
    ((outcome (Inserted_exploding_kitten -10)) "<no specialised alert>")
    ((outcome (Inserted_exploding_kitten -1)) "<no specialised alert>")
    ((outcome (Inserted_exploding_kitten 0)) "<no specialised alert>")
    ((outcome (Inserted_exploding_kitten 1)) "<no specialised alert>")
    ((outcome (Inserted_exploding_kitten 10)) "<no specialised alert>")
    ((outcome (Received_card_from ((Powerless Cattermelon) Somebody)))
     (player Somebody) (alert "Alice received a(n) Cattermelon from you."))
    ((outcome (Saw_the_future ())) "<no specialised alert>")
    ((outcome (Saw_the_future (Exploding_kitten))) "<no specialised alert>")
    ((outcome
      (Saw_the_future
       ((Powerless Tacocat) (Power (Targetless Skip))
        (Power (Targetless See_the_future)))))
     "<no specialised alert>")
    ((outcome Shuffled) "<no specialised alert>")
    ((outcome Skipped) "<no specialised alert>")
    ((outcome
      (Stole_randomly_via_double ((Powerless Cattermelon) Somebody Defuse)))
     (player Somebody)
     (alert
      "Alice played a(n) Cattermelon double and randomly stole a(n) Defuse from you."))
    ((outcome (Stole_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (player Somebody)
     (alert
      "Alice played a(n) Cattermelon triple and stole a(n) Defuse from you."))
    |}]
;;
