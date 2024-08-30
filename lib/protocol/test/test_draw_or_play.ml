open! Core
open Protocol_lib
open Player_hands.For_testing.Hand_or_eliminated

let%expect_test "format doc looks ok" =
  print_string Action.Draw_or_play.format_doc;
  [%expect
    {| draw|CARD|double CARD@TARGET_NAME|triple CARD@TARGET_NAME@TARGET_CARD |}]
;;

let handle_and_print action ~hand ~deck ~other_player_and_hands =
  let player_under_test = Player_name.of_string_exn "player_under_test" in
  let player_hands =
    other_player_and_hands
    |> List.map ~f:(fun (player, hand) ->
      Player_name.of_string_exn player, Hand.of_cards hand |> Playing)
    |> List.append [ player_under_test, Hand.of_cards hand |> Playing ]
    |> Player_hands.For_testing.of_alist_exn
  in
  match
    Action.Draw_or_play.handle
      action
      ~player_hands
      ~player_name:player_under_test
      ~deck:(Deck.For_testing.of_card_list deck)
      ~deterministically:true
  with
  | Error error -> print_s [%message (error : Error.t)]
  | Ok (outcome, player_hands, deck) ->
    print_s
      [%message
        (outcome : Outcome.t) (player_hands : Player_hands.t) (deck : Deck.t)]
;;

let%expect_test "draw from empty deck -> Error" =
  handle_and_print Draw ~hand:[] ~deck:[] ~other_player_and_hands:[];
  [%expect {| (error "Attempting to draw from an empty deck") |}]
;;

let%expect_test "draw Exploding_kitten without defuse -> Exploded" =
  handle_and_print
    Draw
    ~hand:[]
    ~deck:[ Exploding_kitten ]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome Exploded)
     (player_hands ((player_under_test (Playing ((Exploding_kitten 1))))))
     (deck ()))
    |}]
;;

let%expect_test "draw Exploding_kitten with defuse -> Defused and consumed \
                 without holding the exploding kitten"
  =
  handle_and_print
    Draw
    ~hand:[ Defuse ]
    ~deck:[ Exploding_kitten ]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome Defused) (player_hands ((player_under_test (Playing ()))))
     (deck ()))
    |}]
;;

let%expect_test "draw non-Exploding_kitten -> drew successfully" =
  handle_and_print
    Draw
    ~hand:[]
    ~deck:[ Powerless Cattermelon; Exploding_kitten ]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome (Drew_safely (Powerless Cattermelon)))
     (player_hands ((player_under_test (Playing (((Powerless Cattermelon) 1))))))
     (deck (Exploding_kitten)))
    |}]
;;

let%expect_test "play Power without owning -> error" =
  handle_and_print (Play Skip) ~hand:[] ~deck:[] ~other_player_and_hands:[];
  [%expect {| (error ("Card is not owned" (t ()) (card (Power Skip)))) |}]
;;

let%expect_test "play See_the_future with deck of 0 cards -> consumed and 0 \
                 seen"
  =
  handle_and_print
    (Play See_the_future)
    ~hand:[ Power See_the_future ]
    ~deck:[]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome (Saw_the_future ()))
     (player_hands ((player_under_test (Playing ())))) (deck ()))
    |}]
;;

let%expect_test "play See_the_future with deck of 1 card -> consumed and 1 seen"
  =
  handle_and_print
    (Play See_the_future)
    ~hand:[ Power See_the_future ]
    ~deck:[ Exploding_kitten ]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome (Saw_the_future (Exploding_kitten)))
     (player_hands ((player_under_test (Playing ())))) (deck (Exploding_kitten)))
    |}]
;;

let%expect_test "play See_the_future with deck of 4 cards -> consumed and 3 \
                 seen in order"
  =
  handle_and_print
    (Play See_the_future)
    ~hand:[ Power See_the_future ]
    ~deck:
      [ Powerless Beard_cat
      ; Powerless Cattermelon
      ; Powerless Rainbow_ralphing_cat
      ; Power Skip
      ]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome
      (Saw_the_future
       ((Powerless Beard_cat) (Powerless Cattermelon)
        (Powerless Rainbow_ralphing_cat))))
     (player_hands ((player_under_test (Playing ()))))
     (deck
      ((Powerless Beard_cat) (Powerless Cattermelon)
       (Powerless Rainbow_ralphing_cat) (Power Skip))))
    |}]
;;

let%expect_test "play Skip -> consumed and no card drawn" =
  handle_and_print
    (Play Skip)
    ~hand:[ Power Skip ]
    ~deck:[]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome Skipped) (player_hands ((player_under_test (Playing ()))))
     (deck ()))
    |}]
;;

let%expect_test "play Shuffle -> consumed and deck is shuffled" =
  handle_and_print
    (Play Shuffle)
    ~hand:[ Power Shuffle ]
    ~deck:
      [ Powerless Beard_cat
      ; Powerless Cattermelon
      ; Powerless Hairy_potato_cat
      ; Powerless Rainbow_ralphing_cat
      ; Powerless Tacocat
      ; Defuse
      ; Power Skip
      ; Power Shuffle
      ; Power See_the_future
      ; Exploding_kitten
      ]
    ~other_player_and_hands:[];
  [%expect
    {|
    ((outcome Shuffled) (player_hands ((player_under_test (Playing ()))))
     (deck
      (Defuse (Powerless Cattermelon) (Power Skip) (Power See_the_future)
       (Powerless Tacocat) Exploding_kitten (Powerless Rainbow_ralphing_cat)
       (Powerless Hairy_potato_cat) (Power Shuffle) (Powerless Beard_cat))))
    |}]
;;

let%expect_test "play Double Tacocat -> card stolen from target" =
  handle_and_print
    (Double (Powerless Tacocat, Player_name.of_string_exn "target"))
    ~hand:[ Powerless Tacocat; Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[ "target", [ Defuse ] ];
  [%expect
    {|
    ((outcome (Stole_randomly_via_double ((Powerless Tacocat) target Defuse)))
     (player_hands
      ((player_under_test (Playing ((Defuse 1)))) (target (Playing ()))))
     (deck ()))
    |}]
;;

let%expect_test "play Double Tacocat with insufficient Tacocats -> error" =
  handle_and_print
    (Double (Powerless Tacocat, Player_name.of_string_exn "target"))
    ~hand:[ Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[ "target", [ Defuse ] ];
  [%expect
    {|
    (error
     ("Not enough copies owned" (t (((Powerless Tacocat) 1)))
      (card (Powerless Tacocat)) (n 2)))
    |}]
;;

let%expect_test "play Double Tacocat to nonexistent player -> error" =
  handle_and_print
    (Double (Powerless Tacocat, Player_name.of_string_exn "target"))
    ~hand:[ Powerless Tacocat; Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[];
  [%expect
    {|
    (error
     (("Could not find player name" (player_name target)
       (t ((player_under_test (Playing ())))))
      ("key not found" target)))
    |}]
;;

let%expect_test "play Double Tacocat to player with no cards -> error" =
  handle_and_print
    (Double (Powerless Tacocat, Player_name.of_string_exn "target"))
    ~hand:[ Powerless Tacocat; Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[ "target", [] ];
  [%expect {| (error ("Target has an empty hand" (target target))) |}]
;;

let%expect_test "play Triple Tacocat to player with target card Defuse -> 3 \
                 tacocats consumed and card stolen from target"
  =
  handle_and_print
    (Triple (Powerless Tacocat, Player_name.of_string_exn "target", Defuse))
    ~hand:[ Powerless Tacocat; Powerless Tacocat; Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[ "target", [ Defuse ] ];
  [%expect
    {|
    ((outcome (Stole_via_triple ((Powerless Tacocat) target Defuse)))
     (player_hands
      ((player_under_test (Playing ((Defuse 1)))) (target (Playing ()))))
     (deck ()))
    |}]
;;

let%expect_test "play Triple Tacocat to player without target card Defuse -> 3 \
                 tacocats consumed and card failed to steal"
  =
  handle_and_print
    (Triple (Powerless Tacocat, Player_name.of_string_exn "target", Defuse))
    ~hand:[ Powerless Tacocat; Powerless Tacocat; Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[ "target", [ Power Skip ] ];
  [%expect
    {|
    ((outcome (Failed_to_steal_via_triple ((Powerless Tacocat) target Defuse)))
     (player_hands
      ((player_under_test (Playing ())) (target (Playing (((Power Skip) 1))))))
     (deck ()))
    |}]
;;

let%expect_test "play Triple Tacocat with insufficient cards -> error" =
  handle_and_print
    (Triple (Powerless Tacocat, Player_name.of_string_exn "target", Defuse))
    ~hand:[ Powerless Tacocat; Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[ "target", [ Defuse ] ];
  [%expect
    {|
    (error
     ("Not enough copies owned" (t (((Powerless Tacocat) 2)))
      (card (Powerless Tacocat)) (n 3)))
    |}]
;;

let%expect_test "play Triple Tacocat to nonexistent player -> error" =
  handle_and_print
    (Triple
       (Powerless Tacocat, Player_name.of_string_exn "unknown_player", Defuse))
    ~hand:[ Powerless Tacocat; Powerless Tacocat; Powerless Tacocat ]
    ~deck:[]
    ~other_player_and_hands:[ "target", [ Defuse ] ];
  [%expect
    {|
    (error
     (("Could not find player name" (player_name unknown_player)
       (t ((player_under_test (Playing ())) (target (Playing ((Defuse 1)))))))
      ("key not found" unknown_player)))
    |}]
;;
