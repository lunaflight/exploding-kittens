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

(* TODO: This function should probably return an [Or_error.t] if [n] is
   negative or more than the deck size. *)

(** Draws [n] number of cards from the deck. It returns a tuple of the drew
    cards and the resulting deck.
    If [n] is negative, [n] will be set to 0.
    If [n] is more than the deck size, [n] will be set to the deck size. *)
val draw_cards : t -> n:int -> Card.t list * t

(** Draws 1 card from the deck. It returns a tuple of the drew card and the resulting deck.
    If the deck is empty, an error is returned. *)
val draw : t -> (Card.t * t) Or_error.t

module For_testing : sig
  val of_card_list : Card.t list -> t
end
