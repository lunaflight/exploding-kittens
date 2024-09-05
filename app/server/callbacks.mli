open! Core
open! Async
open Protocol_lib

(** This record is involved in [Game_state] to advance the state. Each
    field contains the function associated with what to do based on the name. *)
type t =
  { get_card_to_give :
      player_name:Player_name.t
      -> hand:Hand.t
      -> reprompt_context:string option
      -> Card.t Deferred.t
  ; get_draw_or_play :
      player_name:Player_name.t
      -> hand:Hand.t
      -> reprompt_context:string option
      -> Action.Draw_or_play.t Deferred.t
  ; get_exploding_kitten_insert_position :
      player_name:Player_name.t -> deck_size:int -> int Deferred.t
  ; on_initial_load : player_hands:Player_hands.t -> unit Deferred.t
  ; on_outcome : turn_order:Turn_order.t -> outcome:Outcome.t -> unit Deferred.t
  ; on_win :
      winner:Player_name.t -> spectators:Player_name.t list -> unit Deferred.t
  }

(** Provides a sane default for the required callbacks.
    [connector] is used to handle sending of RPCs. Each field is equipped
    with an RPC to send a message informing the player of some information, and
    receiving respective responses. *)
val default : connector:Connector.t -> t
