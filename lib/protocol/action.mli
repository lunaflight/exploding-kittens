open! Core
open! Async

module Outcome : sig
  type t =
    | Drew of Card.t
    | Exploded
    | Played of Card.Power.t
  [@@deriving sexp_of]

  val to_self_alert : t -> string
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
