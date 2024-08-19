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
    Default presets are used in accordance to the original game. For example,
    each player starts with 1 defuse and draws 7 cards initially. The deck
    composition is also determined by [Deck.default_without_exploding_kittens].

    The deck expects a certain player count. An error will be returned
    if there are too few or too many connections. Refer to
    [Deck.default_without_exploding_kittens].
    An error will also be returned if the deck is not of sufficient size for
    the number of players.

    Additional callback function parameters describe how the server should
    handle the event described by the name. Functions from [Interaction] should
    help in filling these out.

    The [Deferred.t] becomes determined when there is a winner. *)
val start
  :  connections:Rpc.Connection.t list
  -> get_draw_or_play:
       (player:Player.t
        -> reprompt_context:string option
        -> Action.Draw_or_play.t Deferred.t)
  -> get_exploding_kitten_insert_position:
       (player:Player.t -> deck_size:int -> int Deferred.t)
  -> on_outcome:
       (current_player:Player.t
        -> other_players:Player.t list
        -> outcome:Action.Outcome.t
        -> unit Deferred.t)
  -> on_win:(player:Player.t -> message:string -> unit Deferred.t)
  -> unit Or_error.t Deferred.t
