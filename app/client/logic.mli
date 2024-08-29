open! Core
open! Async
open Protocol_lib

val print_string : string -> unit

(** Returns a draw or play action given their [hand]. This will repeatedly
    prompt the client until a valid response. *)
val get_draw_or_play : hand:Hand.t -> Action.Draw_or_play.t Deferred.t

(** Returns an insert position given the [deck_size]. This will repeatedly
    prompt the client until a valid response. *)
val get_exploding_kitten_insert_position : deck_size:int -> int Deferred.t
