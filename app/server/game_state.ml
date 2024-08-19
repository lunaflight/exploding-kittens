open! Core
open! Async
open Protocol_lib
module Next_step = Action.Next_step

module Instant = struct
  type t =
    { deck : Deck.t
    ; current_player : Player.t
    ; other_players : Player.t Nonempty_list.t
    ; next_step : Next_step.t
    }
  [@@deriving fields ~getters]

  let update
    { deck = (_ : Deck.t)
    ; current_player = { connection; hand = (_ : Hand.t); name }
    ; other_players
    ; next_step
    }
    ~deck
    ~hand
    =
    { deck; current_player = { connection; hand; name }; other_players; next_step }
  ;;

  let pass_turn { deck; current_player; other_players; next_step = (_ : Next_step.t) } =
    let (next_player :: tl) = other_players in
    (* The following is equivalent to tl @ [ current_player ] without
         invoking [exn] functions. *)
    let other_players =
      List.rev tl |> Nonempty_list.create current_player |> Nonempty_list.reverse
    in
    { deck; current_player = next_player; other_players; next_step = Draw_or_play }
  ;;
end

type t =
  | Winner of Player.t
  | Ongoing of Instant.t

(* TODO: It could be nice if the player wasn't deleted - maybe dead players
   should be able to see all actions that happen as they spectate. *)
let eliminate_current_player
  ({ deck; current_player = (_ : Player.t); other_players; next_step = (_ : Next_step.t) } :
    Instant.t)
  =
  match other_players with
  | [ player ] -> Winner player
  | current_player :: hd :: tl ->
    Ongoing
      { deck
      ; current_player
      ; other_players = Nonempty_list.create hd tl
      ; next_step = Draw_or_play
      }
;;

(* TODO: If the number of callbacks becomes too high (> 5?), consider linting a
   [Callbacks.t] type. *)
let advance instant ~get_draw_or_play ~get_exploding_kitten_insert_position ~on_outcome =
  match Instant.next_step instant with
  (* TODO: Eliminating players can be more robust - they need not be at the
     front of the queue. *)
  | Eliminate_player -> eliminate_current_player instant |> return
  | Pass_turn -> Instant.pass_turn instant |> Ongoing |> return
  | Insert_exploding_kitten ->
    let%bind position =
      get_exploding_kitten_insert_position
        ~player:instant.current_player
        ~deck_size:(Deck.size instant.deck)
    in
    let outcome, deck =
      Action.Insert_exploding_kitten.handle ~position ~deck:instant.deck
    in
    (* TODO: A hint needs to be added so [on_outcome] is called when there is an
       [outcome]. A good idea is to expose some [Instant] updating function that
       calls that as a side effect. The following is too vulnerable to mistakes.
       There is also some reduplication here. *)
    let%map () =
      on_outcome
        ~current_player:instant.current_player
        ~other_players:(Nonempty_list.to_list instant.other_players)
        ~outcome
    in
    Ongoing { instant with deck; next_step = Action.Next_step.of_outcome outcome }
  | Draw_or_play ->
    (* TODO: It might be a good idea to refactor this interaction part
       elsewhere. It detracts from the main purpose of this function. *)
    let%bind outcome, hand, deck =
      Deferred.repeat_until_finished None (fun reprompt_context ->
        let%map action =
          get_draw_or_play ~player:instant.current_player ~reprompt_context
        in
        match
          Action.Draw_or_play.handle
            action
            ~hand:instant.current_player.hand
            ~deck:instant.deck
        with
        | Error _ -> `Repeat (Some "This action is invalid.")
        | Ok result -> `Finished result)
    in
    let instant = Instant.update instant ~deck ~hand in
    let%map () =
      on_outcome
        ~current_player:instant.current_player
        ~other_players:(Nonempty_list.to_list instant.other_players)
        ~outcome
    in
    Ongoing { instant with next_step = Action.Next_step.of_outcome outcome }
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
  on_win ~player:winner ~message:"You won!"
;;

let start
  ~connections
  ~get_draw_or_play
  ~get_exploding_kitten_insert_position
  ~on_outcome
  ~on_win
  =
  let open Deferred.Or_error.Let_syntax in
  (* TODO: Give a name to this part, perhaps [players_of_connections ~deck]. *)
  let deck, connection_and_hands =
    List.fold_map
      connections
      (* TODO: Provide a way to customise the starting deck and starting hand
         size. *)
      ~init:(Deck.default_without_exploding_kittens () |> Deck.shuffle)
      ~f:(fun deck connection ->
        (* TODO: Properly bind on this [Or_error.t]. *)
        let hand, deck = Deck.draw_hand deck ~n:8 |> Or_error.ok_exn in
        deck, (connection, hand))
  in
  let%bind players =
    Monitor.try_with_or_error (fun () ->
      Deferred.List.map
        ~how:(`Max_concurrent_jobs 16)
        connection_and_hands
        ~f:(fun (connection, hand) ->
          let%map.Deferred name =
            Rpc.Rpc.dispatch Rpcs.Name.rpc connection () |> Deferred.Or_error.ok_exn
          in
          Player.{ connection; hand; name }))
  in
  match List.permute players with
  | [] | [ _ ] ->
    Deferred.Or_error.error_s
      [%message "More than 1 player is required to start the game"]
  | current_player :: hd :: tl ->
    (* TODO: Would be nice to give this initial state a name. *)
    Monitor.try_with_or_error (fun () ->
      { deck = Deck.add_exploding_kittens deck ~n:(List.length players - 1)
      ; current_player
      ; other_players = Nonempty_list.create hd tl
      ; next_step = Draw_or_play
      }
      |> Ongoing
      |> advance_until_win
           ~get_draw_or_play
           ~get_exploding_kitten_insert_position
           ~on_outcome
           ~on_win)
;;
