open! Core

type t =
  | Draw_or_play
  | Eliminate_player
  | Give_turns_via_attacking
  | Insert_exploding_kitten
  | Pass_turn
[@@deriving sexp_of]

let of_outcome =
  Outcome.(
    function
    | Attacked -> Give_turns_via_attacking
    | Defused -> Insert_exploding_kitten
    | Drew_safely _ -> Pass_turn
    | Exploded -> Eliminate_player
    | Failed_to_steal_via_triple _ -> Draw_or_play
    | Inserted_exploding_kitten _ -> Pass_turn
    | Saw_the_future _ -> Draw_or_play
    | Skipped -> Pass_turn
    | Shuffled -> Draw_or_play
    | Stole_via_triple _ -> Draw_or_play
    | Stole_randomly_via_double _ -> Draw_or_play)
;;
