open! Core
open! Async

(** Starts the gameplay loop for the players described by their [connections].
    Sane default presets such as the deck composition and hand size are used. *)
val start : connections:Rpc.Connection.t list -> unit Deferred.t
