open! Core
open! Async
open Protocol_lib

type t =
  { connection : Rpc.Connection.t
  ; hand : Hand.t
  ; name : string
  }

(** Asks the player to provide a draw or play action via RPC. This action may or
    may not be valid depending on the game state. *)
val get_draw_or_play : t -> Action.Draw_or_play.t Deferred.t

(** Asks the player to provide an insert position of an exploding kitten via
    RPC. *)
val get_exploding_kitten_insert_position : t -> deck_size:int -> int Deferred.t

(** Sends a message for the player to process via RPC. *)
val send_message : t -> string -> unit Deferred.t
