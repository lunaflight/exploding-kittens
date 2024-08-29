open! Core
open Regex

let capture_groups_and_print ~case_sensitive ~regex ~string =
  let result = capture_groups ~case_sensitive ~regex ~string in
  print_s [%message (result : string list option Or_error.t)]
;;

let%expect_test "invalid regex -> error" =
  capture_groups_and_print ~case_sensitive:true ~regex:"(" ~string:"";
  [%expect
    {|
    (result
     (Error ("Re2__Regex.Exceptions.Regex_compile_failed(\"missing ): ^($\")")))
    |}]
;;

let%expect_test "2 matches -> matches returned successfully" =
  capture_groups_and_print
    ~case_sensitive:true
    ~regex:"fruits (.*) & (.*)"
    ~string:"fruits apple & banana";
  [%expect {| (result (Ok ((apple banana)))) |}]
;;

let%expect_test "non-matching regex -> None returned" =
  capture_groups_and_print ~case_sensitive:true ~regex:"fruits (.*) & (.*)" ~string:"";
  [%expect {| (result (Ok ())) |}]
;;

let%expect_test "differing only by case and case sensitive -> None returned" =
  capture_groups_and_print
    ~case_sensitive:true
    ~regex:"fruits (.*) & (.*)"
    ~string:"Fruits apple & banana";
  [%expect {| (result (Ok ())) |}]
;;

let%expect_test "differing only by case and case insensitive -> matches returned \
                 successfully"
  =
  capture_groups_and_print
    ~case_sensitive:false
    ~regex:"fruits (.*) & (.*)"
    ~string:"Fruits apple & banana";
  [%expect {| (result (Ok ((apple banana)))) |}]
;;

let%expect_test "regex without capture groups and matches -> Some [] returned \
                 successfully"
  =
  capture_groups_and_print ~case_sensitive:true ~regex:"Fruits?" ~string:"Fruit";
  [%expect {| (result (Ok (()))) |}]
;;

let%expect_test "regex without capture groups and does not match -> None returned" =
  capture_groups_and_print
    ~case_sensitive:true
    ~regex:"Fruits?"
    ~string:"non matching string";
  [%expect {| (result (Ok ())) |}]
;;
