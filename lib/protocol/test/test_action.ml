open! Core
open Protocol_lib

let action_of_string_and_print string =
  let action = Action.Draw_or_play.of_string string in
  print_s [%message (action : Action.Draw_or_play.t Or_error.t)]
;;

let all_mocked_draw_or_plays =
  Action.Draw_or_play.For_testing.all_mocked
    ~double:[ Powerless Cattermelon, Player_name.of_string_exn "Somebody" ]
    ~triple:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
;;

let print_parse_table ~format_f =
  let expected_string_of_action =
    Action.Draw_or_play.(
      function
      | Draw -> "draw"
      | Play See_the_future -> "see the future"
      | Play Skip -> "skip"
      | Play Shuffle -> "shuffle"
      | Double (card, target) ->
        [%string "double %{card#Card}@%{target#Player_name}"]
      | Triple (card, target, target_card) ->
        [%string
          "triple %{card#Card}@%{target#Player_name}@%{target_card#Card}"])
  in
  let parse_table =
    all_mocked_draw_or_plays
    |> List.map ~f:(fun action ->
      let expected_string = action |> expected_string_of_action |> format_f in
      let%map.Or_error action =
        expected_string |> Action.Draw_or_play.of_string
      in
      expected_string, action)
    |> Or_error.all
  in
  print_s
    [%message (parse_table : (string * Action.Draw_or_play.t) list Or_error.t)]
;;

let%expect_test "able to parse all lowercase" =
  print_parse_table ~format_f:String.lowercase;
  [%expect
    {|
    (parse_table
     (Ok
      ((draw Draw) ("see the future" (Play See_the_future)) (skip (Play Skip))
       (shuffle (Play Shuffle))
       ("double cattermelon@somebody"
        (Double ((Powerless Cattermelon) somebody)))
       ("triple cattermelon@somebody@defuse"
        (Triple ((Powerless Cattermelon) somebody Defuse))))))
    |}]
;;

let%expect_test "able to parse all UPPERCASE" =
  print_parse_table ~format_f:String.uppercase;
  [%expect
    {|
    (parse_table
     (Ok
      ((DRAW Draw) ("SEE THE FUTURE" (Play See_the_future)) (SKIP (Play Skip))
       (SHUFFLE (Play Shuffle))
       ("DOUBLE CATTERMELON@SOMEBODY"
        (Double ((Powerless Cattermelon) SOMEBODY)))
       ("TRIPLE CATTERMELON@SOMEBODY@DEFUSE"
        (Triple ((Powerless Cattermelon) SOMEBODY Defuse))))))
    |}]
;;

let%expect_test "able to parse Titlecase" =
  print_parse_table ~format_f:String.capitalize;
  [%expect
    {|
    (parse_table
     (Ok
      ((Draw Draw) ("See the future" (Play See_the_future)) (Skip (Play Skip))
       (Shuffle (Play Shuffle))
       ("Double Cattermelon@Somebody"
        (Double ((Powerless Cattermelon) Somebody)))
       ("Triple Cattermelon@Somebody@Defuse"
        (Triple ((Powerless Cattermelon) Somebody Defuse))))))
    |}]
;;

let%expect_test "unable to parse unknown action" =
  action_of_string_and_print "unknown action";
  [%expect
    {|
    (action
     (Error ("Card.Power.of_string: invalid string" (value "unknown action"))))
    |}]
;;

let%expect_test "unable to parse ill-formed double due to missing string" =
  action_of_string_and_print "double";
  [%expect
    {| (action (Error ("Card.Power.of_string: invalid string" (value double)))) |}]
;;

let%expect_test "unable to parse ill-formed double due to missing fields" =
  action_of_string_and_print "double @";
  [%expect {| (action (Error ("Invalid player name" (string "")))) |}]
;;

let%expect_test "unable to parse ill-formed double due to non-card" =
  action_of_string_and_print "double noncard@player";
  [%expect
    {| (action (Error ("Card.T.of_string: invalid string" (value noncard)))) |}]
;;

let%expect_test "unable to parse ill-formed triple due to missing fields" =
  action_of_string_and_print "triple @@";
  [%expect
    {| (action (Error ("Card.T.of_string: invalid string" (value "")))) |}]
;;
