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
  [%expect {| (parse_table (Ok ((draw Draw) (skip (Play Skip))))) |}]
;;

let%expect_test "able to parse all UPPERCASE" =
  print_parse_table ~format_f:String.uppercase;
  [%expect {| (parse_table (Ok ((DRAW Draw) (SKIP (Play Skip))))) |}]
;;

let%expect_test "able to parse Titlecase" =
  print_parse_table ~format_f:String.capitalize;
  [%expect {| (parse_table (Ok ((Draw Draw) (Skip (Play Skip))))) |}]
;;

let%expect_test "unable to parse unknown action" =
  action_of_string_and_print "unknown action";
  [%expect
    {|
    (action
     (Error ("Action.of_string: invalid string" (value "unknown action"))))
    |}]
;;

let%expect_test "outcome alerts for self look correct - full feedback is given" =
  Action.Outcome.For_testing.all
  |> List.iter ~f:(fun outcome ->
    let alert = Action.Outcome.to_self_alert outcome in
    print_s [%message (outcome : Action.Outcome.t) (alert : string)]);
  [%expect
    {|
    ((outcome (Drew Exploding_kitten)) (alert "You drew a(n) Exploding Kitten."))
    ((outcome (Drew (Power Skip))) (alert "You drew a(n) Skip."))
    ((outcome (Drew (Powerless Beard_cat))) (alert "You drew a(n) Beard Cat."))
    ((outcome (Drew (Powerless Cattermelon)))
     (alert "You drew a(n) Cattermelon."))
    ((outcome (Drew (Powerless Hairy_potato_cat)))
     (alert "You drew a(n) Hairy Potato Cat."))
    ((outcome (Drew (Powerless Rainbow_ralphing_cat)))
     (alert "You drew a(n) Rainbow-ralphing Cat."))
    ((outcome (Drew (Powerless Tacocat))) (alert "You drew a(n) Tacocat."))
    ((outcome Exploded) (alert "You exploded!"))
    ((outcome (Played Skip)) (alert "You played Skip."))
    |}]
;;

let%expect_test "outcome alerts for others look correct - sensitive info is omitted" =
  Action.Outcome.For_testing.all
  |> List.iter ~f:(fun outcome ->
    let alert = Action.Outcome.to_others_alert ~name:"Alice" outcome in
    print_s [%message (outcome : Action.Outcome.t) (alert : string)]);
  [%expect
    {|
    ((outcome (Drew Exploding_kitten)) (alert "Alice drew a card."))
    ((outcome (Drew (Power Skip))) (alert "Alice drew a card."))
    ((outcome (Drew (Powerless Beard_cat))) (alert "Alice drew a card."))
    ((outcome (Drew (Powerless Cattermelon))) (alert "Alice drew a card."))
    ((outcome (Drew (Powerless Hairy_potato_cat))) (alert "Alice drew a card."))
    ((outcome (Drew (Powerless Rainbow_ralphing_cat)))
     (alert "Alice drew a card."))
    ((outcome (Drew (Powerless Tacocat))) (alert "Alice drew a card."))
    ((outcome Exploded) (alert "Alice exploded!"))
    ((outcome (Played Skip)) (alert "Alice played Skip."))
    |}]
;;
