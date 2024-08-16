open! Core
open! Async

module Outcome : sig
  type t =
    | Drew of Card.t
    | Exploded
    | Saw_the_future of Card.t list
    | Skipped
  [@@deriving sexp_of]

  (** Returns an alert to a spectator describing that another player with
      [name] just got an outcome [t].
      Refer to the [.ml] file for what the alert may look like. *)
  val to_others_alert : t -> name:string -> string

  (** Returns an alert to the action performer that they just got an outcome
      [t].
      Refer to the [.ml] file for what the alert may look like. *)
  val to_self_alert : t -> string

  module For_testing : sig
    val all_mocked : drew:Card.t list -> saw_the_future:Card.t list list -> t list
  end
end

type t =
  | Draw
  | Play of Card.Power.t
[@@deriving bin_io, sexp]

(** Returns the outcome of performing an action. The updated hand and deck will
    also be returned. *)
val handle : t -> hand:Hand.t -> deck:Deck.t -> (Outcome.t * Hand.t * Deck.t) Or_error.t

(* TODO: Accomodate shortened forms or unique prefixes of an action. *)
val of_string : string -> t Or_error.t

module For_testing : sig
  val all : t list
end
