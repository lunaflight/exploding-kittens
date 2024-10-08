open! Core
open Protocol_lib

let all_mocked_outcomes =
  Outcome.For_testing.all_mocked
    ~drew_safely:[ Powerless Cattermelon ]
    ~inserted_exploding_kitten:[ 0 ]
    ~failed_to_steal_via_triple:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
    ~favored:[ Player_name.of_string_exn "Somebody" ]
    ~received_card_from:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody" ]
    ~saw_the_future:[ [] ]
    ~stole_via_triple:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
    ~stole_randomly_via_double:
      [ Powerless Cattermelon, Player_name.of_string_exn "Somebody", Defuse ]
;;

let print_next_steps_of_outcomes outcomes =
  outcomes
  |> List.iter ~f:(fun outcome ->
    let next_step = Next_step.of_outcome outcome in
    print_s [%message (outcome : Outcome.t) (next_step : Next_step.t)])
;;

let%expect_test "next step of outcomes" =
  print_next_steps_of_outcomes all_mocked_outcomes;
  [%expect
    {|
    ((outcome Attacked) (next_step Give_turns_via_attacking))
    ((outcome Defused) (next_step Insert_exploding_kitten))
    ((outcome (Drew_safely (Powerless Cattermelon))) (next_step Pass_turn))
    ((outcome Exploded) (next_step Eliminate_player))
    ((outcome
      (Failed_to_steal_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (next_step Draw_or_play))
    ((outcome (Favored Somebody)) (next_step (Receive_card_from Somebody)))
    ((outcome (Inserted_exploding_kitten 0)) (next_step Pass_turn))
    ((outcome (Received_card_from ((Powerless Cattermelon) Somebody)))
     (next_step Draw_or_play))
    ((outcome (Saw_the_future ())) (next_step Draw_or_play))
    ((outcome Shuffled) (next_step Draw_or_play))
    ((outcome Skipped) (next_step Pass_turn))
    ((outcome
      (Stole_randomly_via_double ((Powerless Cattermelon) Somebody Defuse)))
     (next_step Draw_or_play))
    ((outcome (Stole_via_triple ((Powerless Cattermelon) Somebody Defuse)))
     (next_step Draw_or_play))
    |}]
;;
