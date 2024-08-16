open! Core
open! Async

type t [@@deriving bin_io, sexp]

(** Shuffles the deck non-deterministically. *)
val shuffle : t -> t

(** Initialises a shuffled deck with defaults based on the rules of the game.
    There will not be any exploding kittens in this deck. They should be added
    after dealing players their initial hands.
    Refer to the [.ml] file for more details on the card counts. *)
val default_without_exploding_kittens : unit -> t

(** Adds [n] exploding kittens to a deck and shuffles it. *)
val add_exploding_kittens : t -> n:int -> t

(** Draws [n] number of cards from the deck. It returns a tuple of the drew
    cards and the resulting deck.
    If [n] is negative or if [n] is more than the deck size, an error is
    returned. *)
val draw_hand : t -> n:int -> (Hand.t * t) Or_error.t

(** Draws 1 card from the deck. It returns a tuple of the drew card and the resulting deck.
    If the deck is empty, an error is returned. *)
val draw : t -> (Card.t * t) Or_error.t

(** Peeks the top [n] cards of the deck.
    If [n] is more than the deck size, the whole deck is peeked. *)
val peek : t -> n:int -> Card.t list

module For_testing : sig
  val of_card_list : Card.t list -> t
end
