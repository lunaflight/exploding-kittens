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

(** Sends an RPC to the [player] to ask for an [Action.t].
    No validation for the validity of [Action.t] is done, hence reprompting
    is required and described by [reprompt_context]. *)
val get_draw_or_play
  :  player:Player.t
  -> reprompt_context:string option
  -> Action.Draw_or_play.t Deferred.t

(** Sends an RPC to the [player] to ask for an insert position.
    This [int] will be used by [Deck.insert] - refer to its [.mli] for more
    details. *)
val get_exploding_kitten_insert_position
  :  player:Player.t
  -> deck_size:int
  -> int Deferred.t
