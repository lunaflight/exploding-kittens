open! Core
open! Async

type t [@@deriving bin_io, sexp]

val of_cards : Card.t list -> t
val add_card : t -> card:Card.t -> t
val to_string : t -> string
