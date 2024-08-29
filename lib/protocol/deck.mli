open! Core
open! Async

module Without_exploding_kittens : sig
  (** This type represents a deck without any exploding kittens in it.
      [add_exploding_kittens] should be called after. This is for code
      safety. *)
  type t [@@deriving sexp]

  val shuffle : t -> deterministically:bool -> t

  (** Initialises an unshuffled deck with defaults based on the rules of the
      game.
      This deck is suitable for 2 to 5 players. An error is returned if
      [player_cnt] is out of range.
      The deck will be shuffled non-deterministically if [shuffled] is true.

      Refer to the [.ml] file for more details on the card counts. *)
  val default : player_cnt:int -> shuffled:bool -> t Or_error.t

  (** Draws [n] number of cards from the deck. It returns a tuple of the drew
      cards and the resulting deck.
      If [n] is negative or if [n] is more than the deck size, an error is
      returned. *)
  val deal : t -> n:int -> (Hand.t * t) Or_error.t

  module For_testing : sig
    val of_card_list : Card.t list -> t
  end
end

type t [@@deriving bin_io, sexp]

val shuffle : t -> deterministically:bool -> t

(** Adds [player_cnt - 1] exploding kittens to a deck and shuffles it according
    to [deterministically]. This ensures that there will remain 1 player at the
    end of the game who is the winner.

    An error is returned if [player_cnt < 1]. *)
val add_exploding_kittens
  :  Without_exploding_kittens.t
  -> player_cnt:int
  -> deterministically:bool
  -> t Or_error.t

(** Draws 1 card from the deck. It returns a tuple of the drew card and the
    resulting deck.
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
