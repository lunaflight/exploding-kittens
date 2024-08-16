open! Core
open! Async

(** Starts the gameplay loop for the players described by their [connections].
    Sane default presets such as the deck composition and hand size are used.
    An error will be returned if less than 2 [connections] are provided. *)
val start : connections:Rpc.Connection.t list -> unit Or_error.t Deferred.t
