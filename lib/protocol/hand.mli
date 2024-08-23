open! Core
open! Async

type t [@@deriving bin_io, sexp]

val to_string : t -> string
val of_cards : Card.t list -> t
val add_card : t -> card:Card.t -> t

(** Returns [t] after [n] copies of [card] is removed.
    An error is returned if not enough copies of [card] is in [t]. *)
val remove_card : t -> card:Card.t -> n:int -> t Or_error.t

val contains : t -> card:Card.t -> bool

(** Returns a random card from [t] according to [deterministically].
    [None] is returned if the hand is empty. *)
val random_card : t -> deterministically:bool -> Card.t option
