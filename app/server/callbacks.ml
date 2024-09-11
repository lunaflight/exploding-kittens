open! Core
open! Async
open Protocol_lib

type t =
  { get_card_to_give :
      player_name:Player_name.t
      -> hand:Hand.t
      -> reprompt_context:string option
      -> Card.t Deferred.t
  ; get_draw_or_play :
      player_name:Player_name.t
      -> hand:Hand.t
      -> reprompt_context:string option
      -> Action.Draw_or_play.t Deferred.t
  ; get_exploding_kitten_insert_position :
      player_name:Player_name.t -> deck_size:int -> int Deferred.t
  ; on_initial_load :
      player_hands:Player_hands.t -> turn_order:Turn_order.t -> unit Deferred.t
  ; on_outcome : turn_order:Turn_order.t -> outcome:Outcome.t -> unit Deferred.t
  ; on_win :
      winner:Player_name.t -> spectators:Player_name.t list -> unit Deferred.t
  }

let send_messages connector ~players ~messages ~ignore_errors =
  Deferred.Or_error.List.iter
    ~how:(`Max_concurrent_jobs 16)
    players
    ~f:(fun player_name ->
      let sent_message_response =
        Connector.send_messages connector ~player_name ~messages
      in
      match ignore_errors with
      | false -> sent_message_response
      | true ->
        (match%map.Deferred sent_message_response with
         | Ok result -> Or_error.return result
         | Error error ->
           [%log.global.error
             "Attempted to send a message but player disconnected"
               (player_name : Player_name.t)
               (messages : string list)
               (error : Error.t)];
           Or_error.return ()))
;;

let send_message connector ~players ~message ~ignore_errors =
  send_messages connector ~players ~messages:[ message ] ~ignore_errors
;;

let broadcast_outcome_to_players connector ~turn_order ~outcome =
  let open Deferred.Or_error.Let_syntax in
  let current_player = Turn_order.current_player turn_order in
  let%bind () =
    send_message
      connector
      ~players:[ current_player ]
      ~message:(Outcome.to_self_alert outcome)
      ~ignore_errors:false
  in
  let%bind () =
    send_message
      connector
      ~players:(Turn_order.spectators turn_order)
      ~message:(Outcome.to_uncensored_alert outcome ~player_name:current_player)
      ~ignore_errors:true
  in
  match Outcome.to_specialised_alert outcome ~player_name:current_player with
  | None ->
    send_message
      connector
      ~players:(Turn_order.waiting_players turn_order)
      ~message:(Outcome.to_censored_alert outcome ~player_name:current_player)
      ~ignore_errors:false
  | Some (player, alert) ->
    let%bind () =
      send_message
        connector
        ~players:[ player ]
        ~message:alert
        ~ignore_errors:false
    in
    send_message
      connector
      ~players:
        (Turn_order.waiting_players_except turn_order ~blacklist:[ player ])
      ~message:(Outcome.to_censored_alert outcome ~player_name:current_player)
      ~ignore_errors:false
;;

let broadcast_outcome_to_players_exn connector ~turn_order ~outcome =
  broadcast_outcome_to_players connector ~turn_order ~outcome
  |> Deferred.Or_error.ok_exn
;;

let broadcast_win connector ~winner ~spectators =
  Deferred.Or_error.all_unit
    [ send_message
        connector
        ~players:[ winner ]
        ~message:"You won!"
        ~ignore_errors:false
    ; send_message
        connector
        ~players:spectators
        ~message:[%string "%{winner#Player_name} won!"]
        ~ignore_errors:true
    ]
;;

let broadcast_win_exn connector ~winner ~spectators =
  broadcast_win connector ~winner ~spectators |> Deferred.Or_error.ok_exn
;;

let broadcast_dealt_player_hands connector ~player_hands ~turn_order =
  Player_hands.to_playing_alist player_hands
  |> Deferred.Or_error.List.iter
       ~how:(`Max_concurrent_jobs 16)
       ~f:(fun (player, hand) ->
         send_messages
           connector
           ~players:[ player ]
           ~messages:
             [ "The game has started!"
             ; "Turn order:"
             ; [%string "    %{turn_order#Turn_order}"]
             ; "Your hand:"
             ; [%string "    %{hand#Hand}"]
             ]
           ~ignore_errors:false)
;;

let broadcast_dealt_player_hands_exn connector ~player_hands ~turn_order =
  broadcast_dealt_player_hands connector ~player_hands ~turn_order
  |> Deferred.Or_error.ok_exn
;;

let send_player_if_some connector ~player_name ~message =
  match message with
  | None -> Deferred.Or_error.return ()
  | Some message ->
    send_message connector ~players:[ player_name ] ~message ~ignore_errors:true
;;

let get_card_to_give connector ~player_name ~hand ~reprompt_context =
  let open Deferred.Or_error.Let_syntax in
  let%bind () =
    send_player_if_some connector ~player_name ~message:reprompt_context
  in
  Connector.get_card_to_give connector ~player_name ~hand
;;

let get_card_to_give_exn connector ~player_name ~hand ~reprompt_context =
  get_card_to_give connector ~player_name ~hand ~reprompt_context
  |> Deferred.Or_error.ok_exn
;;

let get_draw_or_play connector ~player_name ~hand ~reprompt_context =
  let open Deferred.Or_error.Let_syntax in
  let%bind () =
    send_player_if_some connector ~player_name ~message:reprompt_context
  in
  Connector.get_draw_or_play connector ~player_name ~hand
;;

let get_draw_or_play_exn connector ~player_name ~hand ~reprompt_context =
  get_draw_or_play connector ~player_name ~hand ~reprompt_context
  |> Deferred.Or_error.ok_exn
;;

let get_exploding_kitten_insert_position connector ~player_name ~deck_size =
  Connector.get_exploding_kitten_insert_position
    connector
    ~player_name
    ~deck_size
;;

let get_exploding_kitten_insert_position_exn connector ~player_name ~deck_size =
  get_exploding_kitten_insert_position connector ~player_name ~deck_size
  |> Deferred.Or_error.ok_exn
;;

let default ~connector =
  { get_card_to_give = get_card_to_give_exn connector
  ; get_draw_or_play = get_draw_or_play_exn connector
  ; get_exploding_kitten_insert_position =
      get_exploding_kitten_insert_position_exn connector
  ; on_initial_load = broadcast_dealt_player_hands_exn connector
  ; on_outcome = broadcast_outcome_to_players_exn connector
  ; on_win = broadcast_win_exn connector
  }
;;
