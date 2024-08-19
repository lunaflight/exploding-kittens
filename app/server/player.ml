open! Core
open! Async
open Protocol_lib

type t =
  { connection : Rpc.Connection.t
  ; hand : Hand.t
  ; name : string
  }

let get_draw_or_play { connection; hand; name = (_ : string) } =
  Rpc.Rpc.dispatch Rpcs.Get_draw_or_play.rpc connection hand |> Deferred.Or_error.ok_exn
;;

let get_exploding_kitten_insert_position
  { connection; hand = (_ : Hand.t); name = (_ : string) }
  ~deck_size
  =
  Rpc.Rpc.dispatch Rpcs.Get_exploding_kitten_insert_position.rpc connection deck_size
  |> Deferred.Or_error.ok_exn
;;

let send_message { connection; hand = (_ : Hand.t); name = (_ : string) } message =
  Rpc.Rpc.dispatch Rpcs.Message.rpc connection message |> Deferred.Or_error.ok_exn
;;
