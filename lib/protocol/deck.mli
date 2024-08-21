open! Core
open! Async

type t [@@deriving bin_io, sexp]

val shuffle : t -> deterministically:bool -> t

(** Initialises a shuffled deck based on [deterministically], with defaults
    based on the rules of the game.
    There will not be any exploding kittens in this deck. They should be added
    after dealing players their initial hands with [add_exploding_kittens].
    This deck is suitable for 2 to 5 players. An error is returned if
    [player_cnt] is out of range.

    Refer to the [.ml] file for more details on the card counts. *)
val default_without_exploding_kittens
  :  player_cnt:int
  -> deterministically:bool
  -> t Or_error.t

(* TODO-soon: This should take in a [player_cnt] parameter instead of [n]. *)

(** Adds [n] exploding kittens to a deck and shuffles it according to
    [deterministically]. *)
val add_exploding_kittens : t -> n:int -> deterministically:bool -> t

(* TODO-soon: Lint a [Without_exploding_kittens] module and rename [draw_hand]
   to [deal] under only that module, to make it safer. *)

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

(** Inserts [card] at the [position].
    If [position] is negative, the card is inserted from the back.
    For example:
    If [position] = 0, the card is placed on top.
    If [position] = -1, the card is placed at the bottom.

    It is expected that: -m <= [position] < m.
    If [position] is sufficiently large or small, it is set to [-m] or [m - 1]
    appropriately. *)
val insert : t -> card:Card.t -> position:int -> t

val size : t -> int

module For_testing : sig
  val of_card_list : Card.t list -> t
end
