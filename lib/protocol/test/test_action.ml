open! Core
open Protocol_lib

let action_of_string_and_print string =
  let action = Action.Draw_or_play.of_string string in
  print_s [%message (action : Action.Draw_or_play.t Or_error.t)]
;;

let all_mocked_draw_or_plays =
  Action.Draw_or_play.For_testing.all_mocked
    ~play_targeted_target:(Player_name.of_string_exn "Somebody")
    ~double:[ Powerless Cattermelon, Player_name.of_string_exn "Somebody" ]
    ~triple:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
;;

let print_parse_table ~format_f =
  let expected_string_of_action =
    Action.Draw_or_play.(
      function
      | Draw -> "draw"
      | Play_targeted (Favor, target) -> [%string "favor@%{target#Player_name}"]
      | Play_targetless Attack -> "attack"
      | Play_targetless See_the_future -> "see the future"
      | Play_targetless Skip -> "skip"
      | Play_targetless Shuffle -> "shuffle"
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
      ((draw Draw) (attack (Play_targetless Attack))
       ("see the future" (Play_targetless See_the_future))
       (shuffle (Play_targetless Shuffle)) (skip (Play_targetless Skip))
       (favor@somebody (Play_targeted (Favor somebody)))
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
      ((DRAW Draw) (ATTACK (Play_targetless Attack))
       ("SEE THE FUTURE" (Play_targetless See_the_future))
       (SHUFFLE (Play_targetless Shuffle)) (SKIP (Play_targetless Skip))
       (FAVOR@SOMEBODY (Play_targeted (Favor SOMEBODY)))
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
      ((Draw Draw) (Attack (Play_targetless Attack))
       ("See the future" (Play_targetless See_the_future))
       (Shuffle (Play_targetless Shuffle)) (Skip (Play_targetless Skip))
       (Favor@Somebody (Play_targeted (Favor Somebody)))
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
     (Error
      (("Could not parse string" (string "unknown action"))
       (("Could not parse as variant" (name Triple))
        ("Did not match regex pattern" (regex "triple (.*)@(.*)@(.*)")))
       (("Could not parse as variant" (name Double))
        ("Did not match regex pattern" (regex "double (.*)@(.*)")))
       (("Could not parse as variant" (name Play_targeted))
        ("Did not match regex pattern" (regex "(.*)@(.*)")))
       (("Could not parse as variant" (name Play_targetless))
        ("Card.Power.Targetless.of_string: invalid string"
         (value "unknown action")))
       (("Could not parse as variant" (name Draw))
        ("Did not match regex pattern" (regex draw))))))
    |}]
;;

let%expect_test "unable to parse ill-formed double due to missing string" =
  action_of_string_and_print "double";
  [%expect
    {|
    (action
     (Error
      (("Could not parse string" (string double))
       (("Could not parse as variant" (name Triple))
        ("Did not match regex pattern" (regex "triple (.*)@(.*)@(.*)")))
       (("Could not parse as variant" (name Double))
        ("Did not match regex pattern" (regex "double (.*)@(.*)")))
       (("Could not parse as variant" (name Play_targeted))
        ("Did not match regex pattern" (regex "(.*)@(.*)")))
       (("Could not parse as variant" (name Play_targetless))
        ("Card.Power.Targetless.of_string: invalid string" (value double)))
       (("Could not parse as variant" (name Draw))
        ("Did not match regex pattern" (regex draw))))))
    |}]
;;

let%expect_test "unable to parse ill-formed double due to missing fields" =
  action_of_string_and_print "double @";
  [%expect
    {|
    (action
     (Error
      (("Could not parse string" (string "double @"))
       (("Could not parse as variant" (name Triple))
        ("Did not match regex pattern" (regex "triple (.*)@(.*)@(.*)")))
       (("Could not parse as variant" (name Double))
        ("Card.T.of_string: invalid string" (value "")))
       (("Could not parse as variant" (name Play_targeted))
        ("Card.Power.Targeted.of_string: invalid string" (value "double ")))
       (("Could not parse as variant" (name Play_targetless))
        ("Card.Power.Targetless.of_string: invalid string" (value "double @")))
       (("Could not parse as variant" (name Draw))
        ("Did not match regex pattern" (regex draw))))))
    |}]
;;

let%expect_test "unable to parse ill-formed double due to non-card" =
  action_of_string_and_print "double noncard@player";
  [%expect
    {|
    (action
     (Error
      (("Could not parse string" (string "double noncard@player"))
       (("Could not parse as variant" (name Triple))
        ("Did not match regex pattern" (regex "triple (.*)@(.*)@(.*)")))
       (("Could not parse as variant" (name Double))
        ("Card.T.of_string: invalid string" (value noncard)))
       (("Could not parse as variant" (name Play_targeted))
        ("Card.Power.Targeted.of_string: invalid string"
         (value "double noncard")))
       (("Could not parse as variant" (name Play_targetless))
        ("Card.Power.Targetless.of_string: invalid string"
         (value "double noncard@player")))
       (("Could not parse as variant" (name Draw))
        ("Did not match regex pattern" (regex draw))))))
    |}]
;;

let%expect_test "unable to parse ill-formed triple due to missing fields" =
  action_of_string_and_print "triple @@";
  [%expect
    {|
    (action
     (Error
      (("Could not parse string" (string "triple @@"))
       (("Could not parse as variant" (name Triple))
        ("Card.T.of_string: invalid string" (value "")))
       (("Could not parse as variant" (name Double))
        ("Did not match regex pattern" (regex "double (.*)@(.*)")))
       (("Could not parse as variant" (name Play_targeted))
        ("Card.Power.Targeted.of_string: invalid string" (value "triple @")))
       (("Could not parse as variant" (name Play_targetless))
        ("Card.Power.Targetless.of_string: invalid string" (value "triple @@")))
       (("Could not parse as variant" (name Draw))
        ("Did not match regex pattern" (regex draw))))))
    |}]
;;
