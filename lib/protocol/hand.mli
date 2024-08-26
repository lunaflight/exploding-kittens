open! Core
open! Async

type t [@@deriving bin_io, sexp]

val to_string : t -> string
val of_cards : Card.t list -> t
val add_card : t -> card:Card.t -> t

(** An error is returned if [card] is not in [t]. *)
val remove_card : t -> card:Card.t -> t Or_error.t

val contains : t -> card:Card.t -> bool
