open! Core
open Protocol_lib

let all_mocked_outcomes =
  Outcome.For_testing.all_mocked
    ~drew_safely:[ Powerless Cattermelon ]
    ~inserted_exploding_kitten:[ 0 ]
    ~saw_the_future:[ [] ]
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
    ((outcome Defused) (next_step Insert_exploding_kitten))
    ((outcome (Drew_safely (Powerless Cattermelon))) (next_step Pass_turn))
    ((outcome Exploded) (next_step Eliminate_player))
    ((outcome (Inserted_exploding_kitten 0)) (next_step Pass_turn))
    ((outcome (Saw_the_future ())) (next_step Draw_or_play))
    ((outcome Skipped) (next_step Pass_turn))
    ((outcome Shuffled) (next_step Draw_or_play))
    |}]
;;
