open! Core
open! Async

module Draw_or_play : sig
  type t =
    | Draw
    | Play of Card.Power.t
    | Double of (Card.t * Player_name.t)
    (** Contains the doubled [Card.t], followed by the target [Player_name.t]. *)
    | Triple of (Card.t * Player_name.t * Card.t)
    (** Contains the tripled [Card.t], followed by the target [Player_name.t],
        and the desired [Card.t] to take. *)
  [@@deriving bin_io, sexp]

  (** Returns a format hint doc for how [of_string] should be used. *)
  val format_doc : string

  (** Returns the outcome and updated state of the game, after [player_name]
      performs [t].
      [deterministically] is used to handle deck shuffling if required.
      An error can be returned if [player_name] is not in [player_hands]. *)
  val handle
    :  t
    -> player_hands:Player_hands.t
    -> player_name:Player_name.t
    -> deck:Deck.t
    -> deterministically:bool
    -> (Outcome.t * Player_hands.t * Deck.t) Or_error.t

  (* TODO-someday: Accomodate shortened forms or unique prefixes of an
     action. *)
  val of_string : string -> t Or_error.t
  val to_string : t -> string

  module For_testing : sig
    val all_mocked
      :  double:(Card.t * Player_name.t) list
      -> triple:(Card.t * Player_name.t * Card.t) list
      -> t list
  end
end

module Insert_exploding_kitten : sig
  (** Returns the outcome and updated deck given the [position] of insertion.
      Refer to [Deck.insert] for more details. *)
  val handle : position:int -> deck:Deck.t -> Outcome.t * Deck.t
end
