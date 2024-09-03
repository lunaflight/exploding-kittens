open! Core
open! Async
open Protocol_lib

val print_string : string -> unit

(** Returns a card to give given their [hand]. This will repeatedly
    prompt the client until a known card is given. *)
val get_card_to_give : hand:Hand.t -> Card.t Deferred.t

(** Returns a draw or play action given their [hand]. This will repeatedly
    prompt the client until a known response is given. *)
val get_draw_or_play : hand:Hand.t -> Action.Draw_or_play.t Deferred.t

(** Returns an insert position given the [deck_size]. This will repeatedly
    prompt the client until a known response is given. *)
val get_exploding_kitten_insert_position : deck_size:int -> int Deferred.t
