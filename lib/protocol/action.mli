open! Core
open! Async

module Outcome : sig
  type t =
    | Drew_successfully of Card.t
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

(* TODO: Accomodate shortened forms or unique prefixes of an action. *)
val of_string : string -> t Or_error.t
