open! Core
open! Async
open Protocol_lib

let broadcast_to_players connector ~turn_order ~outcome =
  let open Deferred.Or_error.Let_syntax in
  let send_message players ~message =
    Deferred.Or_error.List.iter
      ~how:(`Max_concurrent_jobs 16)
      players
      ~f:(fun player_name -> Connector.send_message connector ~player_name ~message)
  in
  let current_player = Turn_order.current_player turn_order in
  let%bind () =
    send_message [ current_player ] ~message:(Outcome.to_self_alert outcome)
  in
  match Outcome.to_specialised_alert outcome ~player_name:current_player with
  | None ->
    send_message
      (Turn_order.waiting_players turn_order)
      ~message:(Outcome.to_censored_alert outcome ~player_name:current_player)
  | Some (player, alert) ->
    let%bind () = send_message [ player ] ~message:alert in
    send_message
      (Turn_order.waiting_players_except turn_order ~blacklist:[ player ])
      ~message:(Outcome.to_censored_alert outcome ~player_name:current_player)
;;

let broadcast_to_players_exn connector ~turn_order ~outcome =
  broadcast_to_players connector ~turn_order ~outcome |> Deferred.Or_error.ok_exn
;;

let send_player_if_some connector ~player_name ~message =
  match message with
  | None -> Deferred.Or_error.return ()
  | Some message -> Connector.send_message connector ~player_name ~message
;;

let get_draw_or_play connector ~player_name ~hand ~reprompt_context =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = send_player_if_some connector ~player_name ~message:reprompt_context in
  Connector.get_draw_or_play connector ~player_name ~hand
;;

let get_draw_or_play_exn connector ~player_name ~hand ~reprompt_context =
  get_draw_or_play connector ~player_name ~hand ~reprompt_context
  |> Deferred.Or_error.ok_exn
;;

let get_exploding_kitten_insert_position connector ~player_name ~deck_size =
  Connector.get_exploding_kitten_insert_position connector ~player_name ~deck_size
;;

let get_exploding_kitten_insert_position_exn connector ~player_name ~deck_size =
  get_exploding_kitten_insert_position connector ~player_name ~deck_size
  |> Deferred.Or_error.ok_exn
;;
