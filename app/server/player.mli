open! Core
open! Async
open Protocol_lib

type t =
  { connection : Rpc.Connection.t
  ; hand : Hand.t
  ; name : string
  }

(** Deals [cards_per_player] cards from the [deck] to each connection in
    [Rpc.Connection.t list] and returns the resulting [deck] and [t list].

    An error will be returned if the deck is not sufficiently large enough. *)
val players_of_connections
  :  Rpc.Connection.t list
  -> deck:Deck.Without_exploding_kittens.t
  -> cards_per_player:int
  -> (Deck.Without_exploding_kittens.t * t list) Deferred.Or_error.t

(** Asks the player to provide a draw or play action via RPC. This action may or
    may not be valid depending on the game state. *)
val get_draw_or_play : t -> Action.Draw_or_play.t Deferred.t

(** Asks the player to provide an insert position of an exploding kitten via
    RPC. *)
val get_exploding_kitten_insert_position : t -> deck_size:int -> int Deferred.t

(** Sends a message for the player to process via RPC. *)
val send_message : t -> string -> unit Deferred.t
