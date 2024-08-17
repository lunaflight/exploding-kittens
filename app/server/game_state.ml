open! Core
open! Async
open Protocol_lib

module Instant = struct
  type t =
    { deck : Deck.t
    ; current_player : Player.t
    ; other_players : Player.t Nonempty_list.t
    }
  [@@deriving fields ~getters]

  let update
    { deck = (_ : Deck.t)
    ; current_player = { connection; hand = (_ : Hand.t); name }
    ; other_players
    }
    ~deck
    ~hand
    =
    { deck; current_player = { connection; hand; name }; other_players }
  ;;

  let pass_turn { deck; current_player; other_players } =
    let (next_player :: tl) = other_players in
    (* The following is equivalent to tl @ [ current_player ] without
         invoking [exn] functions. *)
    let other_players =
      List.rev tl |> Nonempty_list.create current_player |> Nonempty_list.reverse
    in
    { deck; current_player = next_player; other_players }
  ;;
end

type t =
  | Winner of Player.t
  | Ongoing of Instant.t

(* TODO: It could be nice if the player wasn't deleted - maybe dead players
   should be able to see all actions that happen as they spectate. *)
let remove_current_player
  ({ deck; current_player = (_ : Player.t); other_players } : Instant.t)
  =
  match other_players with
  | [ player ] -> Winner player
  | current_player :: hd :: tl ->
    Ongoing { deck; current_player; other_players = Nonempty_list.create hd tl }
;;

let advance instant ~get_action ~on_outcome =
  let current_player = Instant.current_player instant in
  let%bind outcome, hand, deck =
    Deferred.repeat_until_finished "Provide an action." (fun prompt ->
      let%map action = get_action ~current_player ~prompt in
      match Action.handle action ~hand:current_player.hand ~deck:instant.deck with
      | Error _ -> `Repeat "This action is invalid."
      | Ok result -> `Finished result)
  in
  let instant = Instant.update instant ~deck ~hand in
  let%map () =
    on_outcome
      ~current_player:instant.current_player
      ~other_players:(Nonempty_list.to_list instant.other_players)
      ~outcome
  in
  match outcome with
  | Action.Outcome.Exploded -> remove_current_player instant
  | Action.Outcome.Drew _ | Action.Outcome.Skipped -> Instant.pass_turn instant |> Ongoing
  | Action.Outcome.Saw_the_future _ -> Ongoing instant
;;

let advance_until_win game_state ~get_action ~on_outcome ~on_win =
  let%bind winner =
    Deferred.repeat_until_finished game_state (function
      | Winner player -> `Finished player |> return
      | Ongoing instant ->
        let%map instant = advance instant ~get_action ~on_outcome in
        `Repeat instant)
  in
  on_win ~player:winner ~message:"You won!"
;;

let start ~connections ~get_action ~on_outcome ~on_win =
  let open Deferred.Or_error.Let_syntax in
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
    Monitor.try_with_or_error (fun () ->
      { deck = Deck.add_exploding_kittens deck ~n:(List.length players - 1)
      ; current_player
      ; other_players = Nonempty_list.create hd tl
      }
      |> Ongoing
      |> advance_until_win ~get_action ~on_outcome ~on_win)
;;
