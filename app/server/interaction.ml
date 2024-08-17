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

let get_action ~current_player ~prompt =
  let%bind () = Player.send_message current_player prompt in
  Player.get_action current_player
;;
