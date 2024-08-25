open! Core

type t =
  | Draw_or_play
  | Eliminate_player
  | Insert_exploding_kitten
  | Pass_turn
[@@deriving sexp_of]

let of_outcome (outcome : Outcome.t) =
  match outcome with
  | Defused -> Insert_exploding_kitten
  | Drew_safely _ -> Pass_turn
  | Exploded -> Eliminate_player
  | Inserted_exploding_kitten _ -> Pass_turn
  | Saw_the_future _ -> Draw_or_play
  | Skipped -> Pass_turn
  | Shuffled -> Draw_or_play
;;
