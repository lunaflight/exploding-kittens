open! Core
open! Async
open Protocol_lib

(** Sends an RPC to all [Player.t]s about the [outcome]. Sensitive data for
    [other_players] will be redacted. *)
val broadcast_to_players
  :  current_player:Player.t
  -> other_players:Player.t list
  -> outcome:Action.Outcome.t
  -> unit Deferred.t

(** Sends an RPC to the [current_player] to ask for an [Action.t]. No
    validation for the [Action.t] is done. It will prompt the player with
    [prompt]. *)
val get_action : current_player:Player.t -> prompt:string -> Action.t Deferred.t
