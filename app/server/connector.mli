open! Core
open! Async
open Protocol_lib

(** This type is an abstraction over being able to send RPCs using
    [Player_name]. *)
type t

(** Initialises [t] after sending an RPC to get their player names.
    An error will be returned if there are duplicate player names. *)
val of_connections : Rpc.Connection.t list -> t Or_error.t Deferred.t

(** Returns all known player names in [t]. *)
val player_names : t -> Player_name.t list

(** Asks the player to provide a card to give via RPC. This card may
    or may not be in the [hand]. *)
val get_card_to_give
  :  t
  -> player_name:Player_name.t
  -> hand:Hand.t
  -> Card.t Or_error.t Deferred.t

(** Asks the player to provide a draw or play action via RPC. This action may
    or may not be valid depending on the game state. *)
val get_draw_or_play
  :  t
  -> player_name:Player_name.t
  -> hand:Hand.t
  -> Action.Draw_or_play.t Or_error.t Deferred.t

(** Asks the player to provide an insert position of an exploding kitten via
    RPC. *)
val get_exploding_kitten_insert_position
  :  t
  -> player_name:Player_name.t
  -> deck_size:int
  -> int Or_error.t Deferred.t

(** Sends messages sequentially for the player to process via multiple RPCs. *)
val send_messages
  :  t
  -> player_name:Player_name.t
  -> messages:string list
  -> unit Or_error.t Deferred.t
