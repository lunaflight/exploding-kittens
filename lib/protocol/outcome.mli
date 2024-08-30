open! Core

type t =
  | Defused
  | Drew_safely of Card.t
  | Exploded
  | Failed_to_steal_via_triple of (Card.t * Player_name.t * Card.t)
  (** Encompasses the played card, followed by the target, followed by the
      target card attempted to be stolen. *)
  | Inserted_exploding_kitten of int
  | Saw_the_future of Card.t list
  | Shuffled
  | Skipped
  | Stole_randomly_via_double of (Card.t * Player_name.t * Card.t)
  (** Encompasses the played card, followed by the target, followed by the
      randomly stolen card. *)
  | Stole_via_triple of (Card.t * Player_name.t * Card.t)
  (** Encompasses the played card, followed by the target, followed by the
      stolen card. *)
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
    -> failed_to_steal_via_triple:(Card.t * Player_name.t * Card.t) list
    -> inserted_exploding_kitten:int list
    -> saw_the_future:Card.t list list
    -> stole_randomly_via_double:(Card.t * Player_name.t * Card.t) list
    -> stole_via_triple:(Card.t * Player_name.t * Card.t) list
    -> t list
end
