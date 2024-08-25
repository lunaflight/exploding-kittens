open! Core
open! Async
open Protocol_lib

module Instant = struct
  type t =
    { deck : Deck.t
    ; player_hands : Player_hands.t
    ; current_player : Player_name.t
        (* TODO-soon: Add a module that encapsulates turn orders and alive players. *)
    ; other_players : Player_name.t Nonempty_list.t
    ; next_step : Next_step.t
    }
  [@@deriving fields ~getters]

  let update
    { deck = (_ : Deck.t); player_hands; current_player; other_players; next_step }
    ~deck
    ~current_player_hand
    =
    { deck
    ; player_hands =
        Player_hands.set_hand_exn
          player_hands
          ~player_name:current_player
          ~hand:current_player_hand
        (* This is fine, as [current_player] is a known player. *)
    ; current_player
    ; other_players
    ; next_step
    }
  ;;

  let pass_turn
    { deck; player_hands; current_player; other_players; next_step = (_ : Next_step.t) }
    =
    let (next_player :: tl) = other_players in
    (* The following is equivalent to tl @ [ current_player ] without
         invoking [exn] functions. *)
    let other_players =
      List.rev tl |> Nonempty_list.create current_player |> Nonempty_list.reverse
    in
    { deck
    ; player_hands
    ; current_player = next_player
    ; other_players
    ; next_step = Draw_or_play
    }
  ;;
end

type t =
  | Winner of Player_name.t
  | Ongoing of Instant.t

let init ~deck ~first_player ~other_players =
  match other_players with
  | [] -> Winner first_player |> Or_error.return
  | hd :: tl ->
    let%map.Or_error deck, player_hands =
      Player_hands.init
        ~player_names:(first_player :: other_players)
        ~deck
        ~cards_per_player:7
        ~deterministically:false
    in
    Ongoing
      { deck
      ; player_hands
      ; current_player = first_player
      ; other_players = Nonempty_list.create hd tl
      ; next_step = Draw_or_play
      }
;;

(* TODO-someday: It could be nice if the player wasn't deleted - maybe dead players
   should be able to see all actions that happen as they spectate. *)
let eliminate_current_player
  ({ deck
   ; player_hands
   ; current_player = (_ : Player_name.t)
   ; other_players
   ; next_step = (_ : Next_step.t)
   } :
    Instant.t)
  =
  match other_players with
  | [ player ] -> Winner player
  | current_player :: hd :: tl ->
    Ongoing
      { deck
      ; player_hands
      ; current_player
      ; other_players = Nonempty_list.create hd tl
      ; next_step = Draw_or_play
      }
;;

(* TODO-someday: If the number of callbacks becomes too high (> 5?), consider linting a
   [Callbacks.t] type. *)
let advance instant ~get_draw_or_play ~get_exploding_kitten_insert_position ~on_outcome =
  match Instant.next_step instant with
  (* TODO-someday: Eliminating players can be more robust - they need not be at the
     front of the queue. *)
  | Eliminate_player -> eliminate_current_player instant |> return
  | Pass_turn -> Instant.pass_turn instant |> Ongoing |> return
  | Insert_exploding_kitten ->
    let%bind position =
      get_exploding_kitten_insert_position
        ~player_name:instant.current_player
        ~deck_size:(Deck.size instant.deck)
    in
    let outcome, deck =
      Action.Insert_exploding_kitten.handle ~position ~deck:instant.deck
    in
    (* TODO-soon: A hint needs to be added so [on_outcome] is called when there is an
       [outcome]. A good idea is to expose some [Instant] updating function that
       calls that as a side effect. The following is too vulnerable to mistakes.
       There is also some reduplication here. *)
    let%map () =
      on_outcome
        ~current_player:instant.current_player
        ~other_players:(Nonempty_list.to_list instant.other_players)
        ~outcome
    in
    Ongoing { instant with deck; next_step = Next_step.of_outcome outcome }
  | Draw_or_play ->
    (* This is fine, as [instant.current_player] is a known player. *)
    let hand =
      Player_hands.hand_exn instant.player_hands ~player_name:instant.current_player
    in
    (* TODO-soon: It might be a good idea to refactor this interaction part
       elsewhere. It detracts from the main purpose of this function. *)
    let%bind outcome, hand, deck =
      Deferred.repeat_until_finished None (fun reprompt_context ->
        let%map action =
          get_draw_or_play ~player_name:instant.current_player ~hand ~reprompt_context
        in
        match
          Action.Draw_or_play.handle
            action
            ~hand
            ~deck:instant.deck
            ~deterministically:false
        with
        | Error _ -> `Repeat (Some "This action is invalid.")
        | Ok result -> `Finished result)
    in
    let instant = Instant.update instant ~deck ~current_player_hand:hand in
    let%map () =
      on_outcome
        ~current_player:instant.current_player
        ~other_players:(Nonempty_list.to_list instant.other_players)
        ~outcome
    in
    Ongoing { instant with next_step = Next_step.of_outcome outcome }
;;

let advance_until_win
  game_state
  ~get_draw_or_play
  ~on_outcome
  ~on_win
  ~get_exploding_kitten_insert_position
  =
  let%bind winner =
    Deferred.repeat_until_finished game_state (function
      | Winner player -> `Finished player |> return
      | Ongoing instant ->
        let%map game_state =
          advance
            instant
            ~get_draw_or_play
            ~get_exploding_kitten_insert_position
            ~on_outcome
        in
        `Repeat game_state)
  in
  on_win ~player_name:winner ~message:"You won!"
;;

let start_game
  ~connector
  ~get_draw_or_play
  ~get_exploding_kitten_insert_position
  ~on_outcome
  ~on_win
  =
  let open Deferred.Or_error.Let_syntax in
  let player_names = Connector.player_names connector in
  (* TODO-someday: Provide a way to customise the starting deck and starting hand
     size. *)
  let%bind deck =
    Deck.Without_exploding_kittens.default
      ~player_cnt:(List.length player_names)
      ~shuffled:true
    |> Deferred.return
  in
  match List.permute player_names with
  | [] | [ _ ] ->
    (* TODO-soon: Add a compile-time guarantee that [connector] has at least 2 players. *)
    Deferred.Or_error.error_s
      [%message "More than 1 player is required to start the game"]
  | first_player :: other_players ->
    let%bind instant = init ~deck ~first_player ~other_players |> Deferred.return in
    Monitor.try_with_or_error (fun () ->
      advance_until_win
        instant
        ~get_draw_or_play
        ~get_exploding_kitten_insert_position
        ~on_outcome
        ~on_win)
;;
