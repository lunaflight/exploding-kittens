open! Core
open Protocol_lib

let%expect_test "Card string representation looks correct" =
  List.iter Card.all ~f:(fun card ->
    let string_representation = Card.to_string card in
    print_s [%message (card : Card.t) (string_representation : string)]);
  [%expect
    {|
    ((card Exploding_kitten) (string_representation "Exploding Kitten"))
    ((card (Power See_the_future)) (string_representation "See The Future"))
    ((card (Power Skip)) (string_representation Skip))
    ((card (Powerless Beard_cat)) (string_representation "Beard Cat"))
    ((card (Powerless Cattermelon)) (string_representation Cattermelon))
    ((card (Powerless Hairy_potato_cat))
     (string_representation "Hairy Potato Cat"))
    ((card (Powerless Rainbow_ralphing_cat))
     (string_representation "Rainbow-ralphing Cat"))
    ((card (Powerless Tacocat)) (string_representation Tacocat))
    |}]
;;
