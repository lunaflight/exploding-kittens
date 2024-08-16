open! Core
open! Async
open Protocol_lib

type t =
  { connection : Rpc.Connection.t
  ; hand : Hand.t
  ; name : string (* TODO: Make use of the name *)
  }

let get_action { connection; hand; name = (_ : string) } =
  Rpc.Rpc.dispatch Rpcs.Get_action.rpc connection hand |> Deferred.Or_error.ok_exn
;;

let send_message { connection; hand = (_ : Hand.t); name = (_ : string) } message =
  Rpc.Rpc.dispatch Rpcs.Message.rpc connection message |> Deferred.Or_error.ok_exn
;;
