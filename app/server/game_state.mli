(* TODO: This module may need testing. Separating this into the pure part
   and sticking it into [lib/protocol] could be worth it to make it testable. *)

(** This module handles interactions with a game state. [Deferred.t]s are
    returned as RPCs may be called to the clients to, for example, send messages. *)

open! Core
open! Async
open Protocol_lib

module Instant : sig
  (** This type represents the instantaneous state of the game. This
      encompasses information like the deck and players. *)
  type t

  (** Returns the player who has to make a move. *)
  val current_player : t -> Player.t
end

type t =
  | Winner of Player.t
  | Ongoing of Instant.t

(** Starts the gameplay loop for the players described by their [connections].
    Sane default presets such as the deck composition and hand size are used.
    An error will be returned if there is less than 2 [connections]. *)
val init : connections:Rpc.Connection.t list -> t Or_error.t Deferred.t

(** Returns the game state after the current player performs the [action].
    An [Or_error.t] is returned if the [action] is not performable in the
    current game state. *)
val advance : Instant.t -> action:Action.t -> t Deferred.t Or_error.t
