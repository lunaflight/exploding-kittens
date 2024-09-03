open! Core
open! Async
open Protocol_lib

module Instant = struct
  type t =
    { deck : Deck.t
    ; player_hands : Player_hands.t
    ; turn_order : Turn_order.t
    ; next_step : Next_step.t
    }
  [@@deriving fields ~getters]

  let update
    { deck = (_ : Deck.t)
    ; player_hands = (_ : Player_hands.t)
    ; turn_order
    ; next_step
    }
    ~deck
    ~player_hands
    =
    { deck; player_hands; turn_order; next_step }
  ;;

  let pass_turn
    { deck; player_hands; turn_order; next_step = (_ : Next_step.t) }
    =
    { deck
    ; player_hands
    ; turn_order = Turn_order.pass_turn turn_order
    ; next_step = Draw_or_play
    }
  ;;

  let give_all_turns_by_attack
    { deck; player_hands; turn_order; next_step = (_ : Next_step.t) }
    ~additional_turns
    =
    { deck
    ; player_hands
    ; turn_order =
        Turn_order.give_all_turns_by_attack turn_order ~additional_turns
    ; next_step = Draw_or_play
    }
  ;;
end

type t =
  | Winner of (Player_name.t * Player_name.t list)
  | Ongoing of Instant.t

let init ~deck ~first_player ~other_players =
  match other_players with
  | [] -> Winner (first_player, other_players) |> Or_error.return
  | second :: others ->
    let%bind.Or_error deck, player_hands =
      Player_hands.init
        ~player_names:(first_player :: other_players)
        ~deck
        ~cards_per_player:7
        ~deterministically:false
    in
    let%map.Or_error turn_order =
      Turn_order.of_player_names ~first:first_player ~second ~others
    in
    Ongoing { deck; player_hands; turn_order; next_step = Draw_or_play }
;;

let eliminate_current_player
  ({ deck; player_hands; turn_order; next_step = (_ : Next_step.t) } :
    Instant.t)
  =
  let current_player = Turn_order.current_player turn_order in
  match Turn_order.eliminate_current_player turn_order with
  | One_left (winner, spectators) -> Winner (winner, spectators)
  | More_than_one_left turn_order ->
    Ongoing
      { deck
      ; player_hands =
          Player_hands.eliminate player_hands ~player_name:current_player
          |> Or_error.ok_exn
          (* This is okay since [current_player] is known. *)
      ; turn_order
      ; next_step = Draw_or_play
      }
;;

(* TODO-soon: Consider linting a [Callbacks.t] type. *)
let advance
  (instant : Instant.t)
  ~get_card_to_give
  ~get_draw_or_play
  ~get_exploding_kitten_insert_position
  ~on_outcome
  =
  let current_player = Turn_order.current_player instant.turn_order in
  (* TODO-someday: Given the reduplication of [on_outcome] and updating of
     game state after, there may be some room for refactoring here.*)
  match Instant.next_step instant with
  (* TODO-someday: Eliminating players can be more robust - they need not be at
     the front of the queue. *)
  | Eliminate_player -> eliminate_current_player instant |> return
  | Give_turns_via_attacking ->
    instant
    |> Instant.give_all_turns_by_attack ~additional_turns:2
    |> Ongoing
    |> return
  | Pass_turn -> Instant.pass_turn instant |> Ongoing |> return
  | Receive_card_from target ->
    let%bind outcome, player_hands =
      Deferred.repeat_until_finished None (fun reprompt_context ->
        (* This is fine, as [target] is a known player. *)
        let hand =
          Player_hands.hand_exn instant.player_hands ~player_name:target
        in
        let%map card =
          get_card_to_give ~player_name:target ~hand ~reprompt_context
        in
        match
          Action.Give_a_card.handle
            ~player_hands:instant.player_hands
            ~receiver:current_player
            ~target
            ~card
        with
        | Error error ->
          `Repeat
            (Some [%string "Received error: %{Error.to_string_hum error}"])
        | Ok result -> `Finished result)
    in
    let%map () = on_outcome ~turn_order:instant.turn_order ~outcome in
    Ongoing
      { instant with player_hands; next_step = Next_step.of_outcome outcome }
  | Insert_exploding_kitten ->
    let%bind position =
      get_exploding_kitten_insert_position
        ~player_name:current_player
        ~deck_size:(Deck.size instant.deck)
    in
    let outcome, deck =
      Action.Insert_exploding_kitten.handle ~position ~deck:instant.deck
    in
    let%map () = on_outcome ~turn_order:instant.turn_order ~outcome in
    Ongoing { instant with deck; next_step = Next_step.of_outcome outcome }
  | Draw_or_play ->
    (* This is fine, as [instant.current_player] is a known player. *)
    let hand =
      Player_hands.hand_exn instant.player_hands ~player_name:current_player
    in
    (* TODO-soon: It might be a good idea to refactor this interaction part
       elsewhere. It detracts from the main purpose of this function. This
       construct is reused in this function, so reduplication needs to be
       reconsidered. *)
    let%bind outcome, player_hands, deck =
      Deferred.repeat_until_finished None (fun reprompt_context ->
        let%map action =
          get_draw_or_play ~player_name:current_player ~hand ~reprompt_context
        in
        match
          Action.Draw_or_play.handle
            action
            ~player_hands:instant.player_hands
            ~player_name:current_player
            ~deck:instant.deck
            ~deterministically:false
        with
        | Error error ->
          `Repeat
            (Some [%string "Received error: %{Error.to_string_hum error}"])
        | Ok result -> `Finished result)
    in
    let instant = Instant.update instant ~deck ~player_hands in
    let%map () = on_outcome ~turn_order:instant.turn_order ~outcome in
    Ongoing { instant with next_step = Next_step.of_outcome outcome }
;;

let start_advancing
  game_state
  ~get_card_to_give
  ~get_draw_or_play
  ~on_initial_load
  ~on_outcome
  ~on_win
  ~get_exploding_kitten_insert_position
  =
  let%bind () =
    match game_state with
    | Winner _ -> Deferred.return ()
    | Ongoing instant -> on_initial_load ~player_hands:instant.player_hands
  in
  let%bind winner, spectators =
    Deferred.repeat_until_finished game_state (function
      | Winner (player, spectators) -> `Finished (player, spectators) |> return
      | Ongoing instant ->
        let%map game_state =
          advance
            instant
            ~get_card_to_give
            ~get_draw_or_play
            ~get_exploding_kitten_insert_position
            ~on_outcome
        in
        `Repeat game_state)
  in
  on_win ~winner ~spectators
;;

let start_game
  ~connector
  ~get_card_to_give
  ~get_draw_or_play
  ~get_exploding_kitten_insert_position
  ~on_initial_load
  ~on_outcome
  ~on_win
  =
  let open Deferred.Or_error.Let_syntax in
  let player_names = Connector.player_names connector in
  (* TODO-someday: Provide a way to customise the starting deck and starting
     hand size. *)
  let%bind deck =
    Deck.Without_exploding_kittens.default
      ~player_cnt:(List.length player_names)
      ~shuffled:true
    |> Deferred.return
  in
  match List.permute player_names with
  | [] | [ _ ] ->
    (* TODO-soon: Add a compile-time guarantee that [connector] has at least 2
       players. *)
    Deferred.Or_error.error_s
      [%message "More than 1 player is required to start the game"]
  | first_player :: other_players ->
    let%bind game_state =
      init ~deck ~first_player ~other_players |> Deferred.return
    in
    Monitor.try_with_or_error (fun () ->
      start_advancing
        game_state
        ~get_card_to_give
        ~get_draw_or_play
        ~get_exploding_kitten_insert_position
        ~on_initial_load
        ~on_outcome
        ~on_win)
;;
