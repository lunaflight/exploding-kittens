open! Core

type t =
  | Draw_or_play
  | Eliminate_player
  | Give_turns_via_attacking
  | Insert_exploding_kitten
  | Pass_turn
  | Receive_card_from of Player_name.t
[@@deriving sexp_of]

val of_outcome : Outcome.t -> t
