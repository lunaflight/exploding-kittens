open! Core
open Protocol_lib

let%expect_test "Card string representation looks correct" =
  List.iter Card.all ~f:(fun card ->
    let string_representation = Card.to_string card in
    print_s [%message (card : Card.t) (string_representation : string)]);
  [%expect
    {|
    ((card Exploding_kitten) (string_representation "Exploding Kitten"))
    ((card (Powerless Beard_cat)) (string_representation "Beard Cat"))
    ((card (Powerless Cattermelon)) (string_representation Cattermelon))
    ((card (Powerless Hairy_potato_cat))
     (string_representation "Hairy Potato Cat"))
    ((card (Powerless Rainbow_ralphing_cat))
     (string_representation "Rainbow-ralphing Cat"))
    ((card (Powerless Tacocat)) (string_representation Tacocat))
    |}]
;;

let%expect_test "String representation of 0 cards looks correct" =
  Card.string_of_cards [] |> print_endline;
  [%expect {||}]
;;

let%expect_test "String representation of 1 card looks correct" =
  Card.string_of_cards [ Powerless Beard_cat ] |> print_endline;
  [%expect {| Beard Cat |}]
;;

let%expect_test "String representation of > 1 card looks correct" =
  Card.string_of_cards [ Powerless Beard_cat; Powerless Cattermelon ] |> print_endline;
  [%expect {| Beard Cat, Cattermelon |}]
;;
