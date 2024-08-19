open! Core
open Protocol_lib

let action_of_string_and_print string =
  let action = Action.Draw_or_play.of_string string in
  print_s [%message (action : Action.Draw_or_play.t Or_error.t)]
;;

let print_parse_table ~format_f =
  let expected_string_of_action =
    let open Action.Draw_or_play in
    function
    | Draw -> "draw"
    | Play Skip -> "skip"
    | Play See_the_future -> "see the future"
  in
  let parse_table =
    Action.Draw_or_play.all
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
     (Error
      ("Action.Draw_or_play.of_string: invalid string" (value "unknown action"))))
    |}]
;;
