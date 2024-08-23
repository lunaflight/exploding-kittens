open! Core
open! Async
open Protocol_lib

let broadcast_to_players connector ~current_player ~other_players ~outcome =
  let open Deferred.Or_error.Let_syntax in
  let%bind () =
    Connector.send_message
      connector
      ~player_name:current_player
      ~message:(Action.Outcome.to_self_alert outcome)
  in
  Deferred.Or_error.List.iter
    ~how:(`Max_concurrent_jobs 16)
    other_players
    ~f:(fun player_name ->
      Connector.send_message
        connector
        ~player_name
        ~message:(Action.Outcome.to_others_alert outcome ~player_name:current_player))
;;

let broadcast_to_players_exn connector ~current_player ~other_players ~outcome =
  broadcast_to_players connector ~current_player ~other_players ~outcome
  |> Deferred.Or_error.ok_exn
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
