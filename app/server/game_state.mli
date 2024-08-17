(* TODO: Consider testing this module. The main obstacle is creating
   [Player.t] without the [Rpc.Connection.t] *)

(** This module handles interactions with a game state. [Deferred.t]s are
    returned as RPCs may be called to the clients to, for example, send messages. *)

open! Core
open! Async
open Protocol_lib

module Instant : sig
  (** This type represents the instantaneous state of the game. This
      encompasses information like the deck and players. *)
  type t
end

type t =
  | Winner of Player.t
  | Ongoing of Instant.t

(** Starts the gameplay loop for the players described by their [connections].
    Sane default presets such as the deck composition and hand size are used.
    An error will be returned if there is less than 2 [connections].

    Additional callback function parameters describe how the server should
    handle the event described by the name. Functions from [Interaction] should
    help in filling these out.

    The [Deferred.t] becomes determined when there is a winner. *)
val start
  :  connections:Rpc.Connection.t list
  -> get_action:(current_player:Player.t -> prompt:string -> Action.t Deferred.t)
  -> on_outcome:
       (current_player:Player.t
        -> other_players:Player.t list
        -> outcome:Action.Outcome.t
        -> unit Deferred.t)
  -> on_win:(player:Player.t -> message:string -> unit Deferred.t)
  -> unit Or_error.t Deferred.t
