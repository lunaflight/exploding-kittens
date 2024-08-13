open! Core
open! Async
open Protocol_lib

val print_string : string -> unit

(** Returns an action given their [hand]. This will repeatedly prompt the
    client.*)
val get_action : hand:Card.t list -> Action.t Deferred.t
