open! Core
open Protocol_lib

let action_of_string_and_print string =
  let action = Action.Draw_or_play.of_string string in
  print_s [%message (action : Action.Draw_or_play.t Or_error.t)]
;;

let all_mocked_draw_or_plays =
  Action.Draw_or_play.For_testing.all_mocked
    ~double_target:(Player_name.of_string "Somebody")
;;

let print_parse_table ~format_f =
  let expected_string_of_action =
    let open Action.Draw_or_play in
    function
    | Draw -> "draw"
    | Play See_the_future -> "see the future"
    | Play Skip -> "skip"
    | Play Shuffle -> "shuffle"
    | Double (card, target) -> [%string "double %{card#Card}@%{target#Player_name}"]
  in
  let parse_table =
    all_mocked_draw_or_plays
    |> List.map ~f:(fun action ->
      let expected_string = action |> expected_string_of_action |> format_f in
      let%map.Or_error action = expected_string |> Action.Draw_or_play.of_string in
      expected_string, action)
    |> Or_error.all
  in
  print_s [%message (parse_table : (string * Action.Draw_or_play.t) list Or_error.t)]
;;

let%expect_test "able to parse all lowercase" =
  print_parse_table ~format_f:String.lowercase;
  [%expect
    {|
    (parse_table
     (Ok
      ((draw Draw) ("see the future" (Play See_the_future)) (skip (Play Skip))
       (shuffle (Play Shuffle))
       ("double defuse@somebody" (Double (Defuse somebody)))
       ("double exploding kitten@somebody" (Double (Exploding_kitten somebody)))
       ("double see the future@somebody"
        (Double ((Power See_the_future) somebody)))
       ("double skip@somebody" (Double ((Power Skip) somebody)))
       ("double shuffle@somebody" (Double ((Power Shuffle) somebody)))
       ("double beard cat@somebody" (Double ((Powerless Beard_cat) somebody)))
       ("double cattermelon@somebody"
        (Double ((Powerless Cattermelon) somebody)))
       ("double hairy potato cat@somebody"
        (Double ((Powerless Hairy_potato_cat) somebody)))
       ("double rainbow-ralphing cat@somebody"
        (Double ((Powerless Rainbow_ralphing_cat) somebody)))
       ("double tacocat@somebody" (Double ((Powerless Tacocat) somebody))))))
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
       ("DOUBLE DEFUSE@SOMEBODY" (Double (Defuse SOMEBODY)))
       ("DOUBLE EXPLODING KITTEN@SOMEBODY" (Double (Exploding_kitten SOMEBODY)))
       ("DOUBLE SEE THE FUTURE@SOMEBODY"
        (Double ((Power See_the_future) SOMEBODY)))
       ("DOUBLE SKIP@SOMEBODY" (Double ((Power Skip) SOMEBODY)))
       ("DOUBLE SHUFFLE@SOMEBODY" (Double ((Power Shuffle) SOMEBODY)))
       ("DOUBLE BEARD CAT@SOMEBODY" (Double ((Powerless Beard_cat) SOMEBODY)))
       ("DOUBLE CATTERMELON@SOMEBODY"
        (Double ((Powerless Cattermelon) SOMEBODY)))
       ("DOUBLE HAIRY POTATO CAT@SOMEBODY"
        (Double ((Powerless Hairy_potato_cat) SOMEBODY)))
       ("DOUBLE RAINBOW-RALPHING CAT@SOMEBODY"
        (Double ((Powerless Rainbow_ralphing_cat) SOMEBODY)))
       ("DOUBLE TACOCAT@SOMEBODY" (Double ((Powerless Tacocat) SOMEBODY))))))
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
       ("Double Defuse@Somebody" (Double (Defuse Somebody)))
       ("Double Exploding Kitten@Somebody" (Double (Exploding_kitten Somebody)))
       ("Double See The Future@Somebody"
        (Double ((Power See_the_future) Somebody)))
       ("Double Skip@Somebody" (Double ((Power Skip) Somebody)))
       ("Double Shuffle@Somebody" (Double ((Power Shuffle) Somebody)))
       ("Double Beard Cat@Somebody" (Double ((Powerless Beard_cat) Somebody)))
       ("Double Cattermelon@Somebody"
        (Double ((Powerless Cattermelon) Somebody)))
       ("Double Hairy Potato Cat@Somebody"
        (Double ((Powerless Hairy_potato_cat) Somebody)))
       ("Double Rainbow-ralphing Cat@Somebody"
        (Double ((Powerless Rainbow_ralphing_cat) Somebody)))
       ("Double Tacocat@Somebody" (Double ((Powerless Tacocat) Somebody))))))
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
