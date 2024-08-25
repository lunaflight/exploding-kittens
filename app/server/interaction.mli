open! Core
open! Async
open Protocol_lib

(** Sends an RPC to all [Player_name.t]s about the [outcome]. Sensitive data for
    [other_players] will be redacted. *)
val broadcast_to_players_exn
  :  Connector.t
  -> current_player:Player_name.t
  -> other_players:Player_name.t list
  -> outcome:Outcome.t
  -> unit Deferred.t

(** Sends an RPC to the [player_name] to ask for an [Action.t].
    No validation for the validity of [Action.t] is done, hence reprompting
    is required and described by [reprompt_context]. *)
val get_draw_or_play_exn
  :  Connector.t
  -> player_name:Player_name.t
  -> hand:Hand.t
  -> reprompt_context:string option
  -> Action.Draw_or_play.t Deferred.t

(** Sends an RPC to the [player_name] to ask for an insert position.
    This [int] will be used by [Deck.insert] - refer to its [.mli] for more
    details. *)
val get_exploding_kitten_insert_position_exn
  :  Connector.t
  -> player_name:Player_name.t
  -> deck_size:int
  -> int Deferred.t
