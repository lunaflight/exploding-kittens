open! Core
open! Async
open Protocol_lib

let broadcast_to_players ~current_player ~other_players ~outcome =
  let%bind () =
    Action.Outcome.to_self_alert outcome |> Player.send_message current_player
  in
  Deferred.List.iter ~how:(`Max_concurrent_jobs 16) other_players ~f:(fun player ->
    Action.Outcome.to_others_alert outcome ~name:current_player.name
    |> Player.send_message player)
;;

let send_player_if_some player ~message =
  match message with
  | None -> Deferred.return ()
  | Some message -> Player.send_message player message
;;

let get_draw_or_play ~player ~reprompt_context =
  let%bind () = send_player_if_some player ~message:reprompt_context in
  Player.get_draw_or_play player
;;

let get_exploding_kitten_insert_position ~player ~deck_size =
  Player.get_exploding_kitten_insert_position player ~deck_size
;;
