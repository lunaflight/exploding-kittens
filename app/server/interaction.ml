open! Core
open! Async

let rec get_action_and_advance instant ~prompt =
  let player = Game_state.Instant.current_player instant in
  let%bind () = Player.send_message player prompt in
  let%bind action = Player.get_action player in
  match Game_state.advance instant ~action with
  | Error _ -> get_action_and_advance instant ~prompt:"Cannot perform action, try again."
  | Ok game_state -> game_state
;;

let rec advance_until_win (game_state : Game_state.t) =
  match game_state with
  | Winner player -> Player.send_message player "You won!"
  | Ongoing instant ->
    let%bind game_state = get_action_and_advance instant ~prompt:"Provide an action." in
    advance_until_win game_state
;;

let start ~connections =
  let open Deferred.Or_error.Let_syntax in
  let%bind game_state = Game_state.init ~connections in
  Monitor.try_with_or_error (fun () -> advance_until_win game_state)
;;
