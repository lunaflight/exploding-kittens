open! Core
open! Async
open Protocol_lib

type t =
  { connection : Rpc.Connection.t
  ; hand : Hand.t
  ; name : string
  }

(** Asks the player to provide an action via RPC. This action may or may not be
    valid depending on the game state. *)
val get_action : t -> Action.t Deferred.t

(** Sends a message for the player to process via RPC. *)
val send_message : t -> string -> unit Deferred.t
