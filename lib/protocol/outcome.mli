open! Core

type t =
  | Defused
  | Drew_safely of Card.t
  | Exploded
  | Inserted_exploding_kitten of int
  | Saw_the_future of Card.t list
  | Skipped
  | Shuffled
  | Stole_randomly of (Card.t * Player_name.t)
  (** The [Player_name.t] is the target of the steal. *)
[@@deriving sexp_of]

(** Returns an alert to the action performer that they just got an outcome
    [t].
    Refer to the [.ml] file for what the alert may look like. *)
val to_self_alert : t -> string

(** Returns the specialised alert along with the player it is meant for, if it
    exists.
    It describes that another player with [player_name] just got an outcome
    [t].

    An example is if [t] is [Stole_randomly] - the [target] deserves a special
    message.
    Refer to the [.ml] file for what the alert may look like. *)
val to_specialised_alert
  :  t
  -> player_name:Player_name.t
  -> (Player_name.t * string) option

(** Returns an alert to other players describing that another player with
    [player_name] just got an outcome [t].
    Refer to the [.ml] file for what the alert may look like. *)
val to_censored_alert : t -> player_name:Player_name.t -> string

(** Returns an alert to spectators describing that a player with
    [player_name] just got an outcome [t].
    Refer to the [.ml] file for what the alert may look like. *)
val to_uncensored_alert : t -> player_name:Player_name.t -> string

module For_testing : sig
  val all_mocked
    :  drew_safely:Card.t list
    -> inserted_exploding_kitten:int list
    -> saw_the_future:Card.t list list
    -> stole_randomly:(Card.t * Player_name.t) list
    -> t list
end
