open! Core
open! Async

module Outcome : sig
  type t =
    | Drew_successfully
    | Exploded
  [@@deriving sexp_of]
end

type t = Draw [@@deriving bin_io, sexp]

(** Returns the outcome of performing an action. The updated hand and deck will
    also be returned. *)
val handle
  :  t
  -> hand:Card.t list
  -> deck:Deck.t
  -> (Outcome.t * Card.t list * Deck.t) Or_error.t
