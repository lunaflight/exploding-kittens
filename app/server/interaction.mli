open! Core
open! Async
open Protocol_lib

(** Sends a personalised RPC to all [Player_name.t]s about the [outcome].
    [turn_order] will be used to determine the [current_player] and how much
    information each player should be told. *)
val broadcast_outcome_to_players_exn
  :  Connector.t
  -> turn_order:Turn_order.t
  -> outcome:Outcome.t
  -> unit Deferred.t

(** Sends a message to [winner] and [spectators] about [winner] winning via
    RPC. *)
val broadcast_win_exn
  :  Connector.t
  -> winner:Player_name.t
  -> spectators:Player_name.t list
  -> unit Deferred.t

(** Sends an initial message to all playing [Player_name.t]s in
    [Player_hands.t] after an initial deal via RPC. *)
val broadcast_dealt_player_hands_exn
  :  Connector.t
  -> player_hands:Player_hands.t
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
