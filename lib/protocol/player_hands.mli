open! Core

(** This type is an abstraction over a mapping from [Player_name.t] to
    [Hand.t].
    It also keeps track of whether [Player_name.t] is eliminated and cannot be
    interacted with. *)
type t [@@deriving sexp_of]

(** Deals [cards_per_player] cards to each player in [player_names] from
    [deck].
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
    Returns an error if [player_name] is unknown or if they are eliminated. *)
val hand_or_error : t -> player_name:Player_name.t -> Hand.t Or_error.t

(** Equivalent to [hand_or_error ... |> Or_error.is_ok]. *)
val is_playing : t -> player_name:Player_name.t -> bool

(** See [hand_or_error] for when it will throw an exception. *)
val hand_exn : t -> player_name:Player_name.t -> Hand.t

(* TODO-soon: After implementing [Steal], remove what's not needed here. *)

(** Sets [player_name]'s hand to [hand].
    Returns an error if [player_name] is unknown or if they are eliminated. *)
val set_hand : t -> player_name:Player_name.t -> hand:Hand.t -> t Or_error.t

(** See [set_hand] for when it will throw an exception. *)
val set_hand_exn : t -> player_name:Player_name.t -> hand:Hand.t -> t

(** Returns an error if [player_name] is unknown or if they are eliminated. *)
val add_card : t -> player_name:Player_name.t -> card:Card.t -> t Or_error.t

(** Returns an error if [player_name] is unknown or if they are eliminated. *)
val remove_card
  :  t
  -> player_name:Player_name.t
  -> card:Card.t
  -> n:int
  -> t Or_error.t

(** Returns an error if [player_name] is unknown or if they are eliminated. *)
val has_card : t -> player_name:Player_name.t -> card:Card.t -> bool Or_error.t

(** Transfers [card] from [target] to [receiver].

    An error is returned if:
    - [target] does not have the card
    - [receiver] or [target] is not a known player name
    - [receiver] or [target] is eliminated
    - [receiver] and [target] are the same player name *)
val transfer_card
  :  t
  -> receiver:Player_name.t
  -> target:Player_name.t
  -> card:Card.t
  -> t Or_error.t

(** Equivalent to [transfer_card t ~receiver ~target ~card] where [card] is
    chosen from the [target]'s hand according to [deterministically].
    It returns the randomly chosen card followed by what is returned by
    [transfer_card].

    An error is returned if [target] has no cards or [transfer_card] returns
    an error. *)
val transfer_random_card
  :  t
  -> receiver:Player_name.t
  -> target:Player_name.t
  -> deterministically:bool
  -> (Card.t * t) Or_error.t

(** Eliminates [player_name], making the player not interactable with.
    If [player_name] is already eliminated, this function does nothing.
    An error is returned if [player_name] is unknown. *)
val eliminate : t -> player_name:Player_name.t -> t Or_error.t

(** Returns an association list of all playing [Player_name.t]s with their
    [Hand.t]s. *)
val to_playing_alist : t -> (Player_name.t * Hand.t) list

module For_testing : sig
  module Hand_or_eliminated : sig
    type t =
      | Eliminated
      | Playing of Hand.t
  end

  val of_alist_exn : (Player_name.t * Hand_or_eliminated.t) list -> t
end
