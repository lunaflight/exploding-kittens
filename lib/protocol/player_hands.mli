(** TODO-soon: This module should also encapsulate a [Deck.t] as many
    operations like drawing deal with this as a group and return it as a group. *)
open! Core

(** This type is an abstraction over a mapping from [Player_name.t] to [Hand.t]. *)
type t [@@deriving sexp_of]

(** Deals [cards_per_player] cards to each player in [player_names] from [deck].
    The deck will be shuffled according to [deterministically].

    Returns an error if:
    - the deck is not sufficiently big enough
    - [cards_per_player] is negative
    - There are duplicate player names in [player_names] *)
val init
  :  player_names:Player_name.t list
  -> deck:Deck.Without_exploding_kittens.t
  -> cards_per_player:int
  -> deterministically:bool
  -> (Deck.t * t) Or_error.t

(** Returns the [Hand.t] belonging to [player_name].
    Returns an error if [player_name] is unknown. *)
val hand : t -> player_name:Player_name.t -> Hand.t Or_error.t

(** See [hand_or_error] for when it will throw an exception. *)
val hand_exn : t -> player_name:Player_name.t -> Hand.t

(* TODO-soon: After implementing [Steal], [Double] and [Triple], remove what's not needed here. *)

(** Sets [player_name]'s hand to [hand].
    Returns an error if [player_name] is unknown. *)
val set_hand : t -> player_name:Player_name.t -> hand:Hand.t -> t Or_error.t

(** See [set_hand] for when it will throw an exception. *)
val set_hand_exn : t -> player_name:Player_name.t -> hand:Hand.t -> t

(** Returns an error if [player_name] is unknown. *)
val add_card : t -> player_name:Player_name.t -> card:Card.t -> t Or_error.t

(** Returns an error if [player_name] is unknown. *)
val remove_card : t -> player_name:Player_name.t -> card:Card.t -> t Or_error.t

(** Returns an error if [player_name] is unknown. *)
val has_card : t -> player_name:Player_name.t -> card:Card.t -> bool Or_error.t

module For_testing : sig
  val of_alist_exn : (Player_name.t * Hand.t) list -> t
end
