open! Core
open Protocol_lib

let action_of_string_and_print string =
  let action = Action.of_string string in
  print_s [%message (action : Action.t Or_error.t)]
;;

let print_parse_table ~format_f =
  let expected_string_of_action =
    let open Action in
    function
    | Draw -> "draw"
    | Play Skip -> "skip"
    | Play See_the_future -> "see the future"
  in
  let parse_table =
    Action.For_testing.all
    |> List.map ~f:(fun action ->
      let expected_string = action |> expected_string_of_action |> format_f in
      let%map.Or_error action = expected_string |> Action.of_string in
      expected_string, action)
    |> Or_error.all
  in
  print_s [%message (parse_table : (string * Action.t) list Or_error.t)]
;;

let%expect_test "able to parse all lowercase" =
  print_parse_table ~format_f:String.lowercase;
  [%expect
    {|
    (parse_table
     (Ok
      ((draw Draw) ("see the future" (Play See_the_future)) (skip (Play Skip)))))
    |}]
;;

let%expect_test "able to parse all UPPERCASE" =
  print_parse_table ~format_f:String.uppercase;
  [%expect
    {|
    (parse_table
     (Ok
      ((DRAW Draw) ("SEE THE FUTURE" (Play See_the_future)) (SKIP (Play Skip)))))
    |}]
;;

let%expect_test "able to parse Titlecase" =
  print_parse_table ~format_f:String.capitalize;
  [%expect
    {|
    (parse_table
     (Ok
      ((Draw Draw) ("See the future" (Play See_the_future)) (Skip (Play Skip)))))
    |}]
;;

let%expect_test "unable to parse unknown action" =
  action_of_string_and_print "unknown action";
  [%expect
    {|
    (action
     (Error ("Action.of_string: invalid string" (value "unknown action"))))
    |}]
;;

let all_mocked_outcomes =
  Action.Outcome.For_testing.all_mocked
    ~drew:[ Powerless Cattermelon ]
    ~saw_the_future:
      [ []
      ; [ Exploding_kitten ]
      ; [ Powerless Tacocat; Power Skip; Power See_the_future ]
      ]
;;

let%expect_test "outcome alerts for self look correct - full feedback is given" =
  all_mocked_outcomes
  |> List.iter ~f:(fun outcome ->
    let alert = Action.Outcome.to_self_alert outcome in
    print_s [%message (outcome : Action.Outcome.t) (alert : string)]);
  [%expect
    {|
    ((outcome (Drew (Powerless Cattermelon)))
     (alert "You drew a(n) Cattermelon."))
    ((outcome Exploded) (alert "You exploded!"))
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
  all_mocked_outcomes
  |> List.iter ~f:(fun outcome ->
    let alert = Action.Outcome.to_others_alert ~name:"Alice" outcome in
    print_s [%message (outcome : Action.Outcome.t) (alert : string)]);
  [%expect
    {|
    ((outcome (Drew (Powerless Cattermelon))) (alert "Alice drew a card."))
    ((outcome Exploded) (alert "Alice exploded!"))
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
