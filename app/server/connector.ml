open! Core
open! Async
open Protocol_lib

type t = Rpc.Connection.t Player_name.Map.t

let of_connections connections =
  let open Deferred.Or_error.Let_syntax in
  let%bind name_connection_alist =
    Deferred.Or_error.List.map
      ~how:(`Max_concurrent_jobs 16)
      connections
      ~f:(fun connection ->
        let%map player_name =
          Rpc.Rpc.dispatch Rpcs.Player_name.rpc connection ()
        in
        player_name, connection)
  in
  Player_name.Map.of_alist_or_error name_connection_alist
  |> Or_error.tag ~tag:"Multiple players have the same name"
  |> Deferred.return
;;

let player_names = Map.keys
let to_connection t ~player_name = Map.find_or_error t player_name

let get_draw_or_play t ~player_name ~hand =
  let open Deferred.Or_error.Let_syntax in
  let%bind connection = to_connection t ~player_name |> Deferred.return in
  Rpc.Rpc.dispatch Rpcs.Get_draw_or_play.rpc connection hand
;;

let get_exploding_kitten_insert_position t ~player_name ~deck_size =
  let open Deferred.Or_error.Let_syntax in
  let%bind connection = to_connection t ~player_name |> Deferred.return in
  Rpc.Rpc.dispatch
    Rpcs.Get_exploding_kitten_insert_position.rpc
    connection
    deck_size
;;

let send_message t ~player_name ~message =
  let open Deferred.Or_error.Let_syntax in
  let%bind connection = to_connection t ~player_name |> Deferred.return in
  Rpc.Rpc.dispatch Rpcs.Message.rpc connection message
;;
