open! Core

type t =
  | Defused
  | Drew_safely of Card.t
  | Exploded
  | Inserted_exploding_kitten of int
  | Saw_the_future of Card.t list
  | Skipped
  | Shuffled
[@@deriving sexp_of]

(** Returns an alert to a spectator describing that another player with
    [player_name] just got an outcome [t].
    Refer to the [.ml] file for what the alert may look like. *)
val to_others_alert : t -> player_name:Player_name.t -> string

(** Returns an alert to the action performer that they just got an outcome
    [t].
    Refer to the [.ml] file for what the alert may look like. *)
val to_self_alert : t -> string

module For_testing : sig
  val all_mocked
    :  drew_safely:Card.t list
    -> inserted_exploding_kitten:int list
    -> saw_the_future:Card.t list list
    -> t list
end
