open! Core
open Protocol_lib

let action_of_string_and_print string =
  let action = Action.of_string string in
  print_s [%message (action : Action.t Or_error.t)]
;;

let%expect_test "able to parse all lowercase" =
  action_of_string_and_print "draw";
  [%expect {| (action (Ok Draw)) |}]
;;

let%expect_test "able to parse all uppercase" =
  action_of_string_and_print "DRAW";
  [%expect {| (action (Ok Draw)) |}]
;;

let%expect_test "able to parse mixed case" =
  action_of_string_and_print "dRaW";
  [%expect {| (action (Ok Draw)) |}]
;;

let%expect_test "unable to parse unknown action" =
  action_of_string_and_print "unknown action";
  [%expect
    {|
    (action
     (Error ("Action.of_string: invalid string" (value "unknown action"))))
    |}]
;;
