(** This module handles interactions with a game state. [Deferred.t]s are
    returned as RPCs may be called to the clients to, for example, send
    messages. *)

open! Core
open! Async
open Protocol_lib

module Instant : sig
  (** This type represents the instantaneous state of the game. This
      encompasses information like the deck and players. *)
  type t
end

type t =
  | Winner of (Player_name.t * Player_name.t list)
  (** Contains the winner, followed by all other players. *)
  | Ongoing of Instant.t

(** Starts the gameplay loop for the players encapsulated by [connector].
    Default presets are used in accordance to the original game. For example,
    each player starts with 1 defuse and draws 7 cards initially. The deck
    composition is also determined by [Deck.default_without_exploding_kittens].

    The deck expects a certain player count. An error will be returned if there
    are too few or too many connections. Refer to
    [Deck.default_without_exploding_kittens].

    Additional callback function parameters describe how the server should
    handle the event described by the name. Functions from [Interaction] should
    help in filling these out.

    The [Deferred.t] becomes determined when there is a winner. *)
val start_game
  :  connector:Connector.t
  -> get_draw_or_play:
       (player_name:Player_name.t
        -> hand:Hand.t
        -> reprompt_context:string option
        -> Action.Draw_or_play.t Deferred.t)
  -> get_exploding_kitten_insert_position:
       (player_name:Player_name.t -> deck_size:int -> int Deferred.t)
  -> on_initial_load:(player_hands:Player_hands.t -> unit Deferred.t)
  -> on_outcome:
       (turn_order:Turn_order.t -> outcome:Outcome.t -> unit Deferred.t)
  -> on_win:
       (winner:Player_name.t
        -> spectators:Player_name.t list
        -> unit Deferred.t)
  -> unit Or_error.t Deferred.t
