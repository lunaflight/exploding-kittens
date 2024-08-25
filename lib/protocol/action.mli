open! Core
open! Async

module Draw_or_play : sig
  type t =
    | Draw
    | Play of Card.Power.t
  [@@deriving bin_io, sexp]

  (** Returns the outcome, updated hand and updated deck given the parameters.
      [deterministically] is used to handle deck shuffling if required. *)
  val handle
    :  t
    -> hand:Hand.t
    -> deck:Deck.t
    -> deterministically:bool
    -> (Outcome.t * Hand.t * Deck.t) Or_error.t

  (* TODO-someday: Accomodate shortened forms or unique prefixes of an action. *)
  val of_string : string -> t Or_error.t
  val to_string : t -> string
  val all : t list
end

module Insert_exploding_kitten : sig
  (** Returns the outcome and updated deck given the [position] of insertion.
      Refer to [Deck.insert] for more details. *)
  val handle : position:int -> deck:Deck.t -> Outcome.t * Deck.t
end
