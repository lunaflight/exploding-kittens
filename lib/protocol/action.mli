open! Core
open! Async

module Outcome : sig
  type t =
    | Defused
    | Drew_safely of Card.t
    | Exploded
    | Inserted_exploding_kitten of int
    | Saw_the_future of Card.t list
    | Skipped
  [@@deriving sexp_of]

  (** Returns an alert to a spectator describing that another player with
      [name] just got an outcome [t].
      Refer to the [.ml] file for what the alert may look like. *)
  val to_others_alert : t -> name:string -> string

  (** Returns an alert to the action performer that they just got an outcome
      [t].
      Refer to the [.ml] file for what the alert may look like. *)
  val to_self_alert : t -> string

  module For_testing : sig
    val all_mocked
      :  drew_safely:Card.t list
      -> inserted_exploding_kitten:int list
      -> saw_the_future:Card.t list list
      -> t list
  end
end

module Next_step : sig
  type t =
    | Draw_or_play
    | Eliminate_player
    | Insert_exploding_kitten
    | Pass_turn
  [@@deriving sexp_of]

  val of_outcome : Outcome.t -> t
end

module Draw_or_play : sig
  type t =
    | Draw
    | Play of Card.Power.t
  [@@deriving bin_io, sexp]

  (** Returns the outcome, updated hand and updated deck given the parameters. *)
  val handle : t -> hand:Hand.t -> deck:Deck.t -> (Outcome.t * Hand.t * Deck.t) Or_error.t

  (* TODO: Accomodate shortened forms or unique prefixes of an action. *)
  val of_string : string -> t Or_error.t
  val to_string : t -> string
  val all : t list
end

module Insert_exploding_kitten : sig
  (** Returns the outcome and updated deck given the [position] of insertion.
      Refer to [Deck.insert] for more details. *)
  val handle : position:int -> deck:Deck.t -> Outcome.t * Deck.t
end
