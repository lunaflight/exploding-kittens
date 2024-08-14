open! Core
open! Async
open Protocol_lib

val print_string : string -> unit

(** Returns an action given their [hand]. This will repeatedly prompt the
    client until a valid response. *)
val get_action : hand:Hand.t -> Action.t Deferred.t
