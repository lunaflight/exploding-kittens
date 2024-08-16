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

  let rotate_players { deck; current_player; other_players } =
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

let remove_current_player
  ({ deck; current_player = (_ : Player.t); other_players } : Instant.t)
  =
  match other_players with
  | [ player ] -> Winner player
  | current_player :: hd :: tl ->
    Ongoing { deck; current_player; other_players = Nonempty_list.create hd tl }
;;

let advance instant ~action =
  let current_player = Instant.current_player instant in
  let%map.Or_error outcome, hand, deck =
    Action.handle action ~hand:current_player.hand ~deck:instant.deck
  in
  let%map () =
    Action.Outcome.to_self_alert outcome |> Player.send_message current_player
  in
  match outcome with
  | Action.Outcome.Exploded -> remove_current_player instant
  | Action.Outcome.Drew _ | Action.Outcome.Played Skip ->
    Instant.update instant ~deck ~hand |> Instant.rotate_players |> Ongoing
;;

let init ~connections =
  let deck, connection_and_hands =
    List.fold_map
      connections
      (* TODO: Provide a way to customise the starting deck and starting hand
         size. *)
      ~init:(Deck.default_without_exploding_kittens () |> Deck.shuffle)
      ~f:(fun deck connection ->
        (* TODO: Properly handle re-prompting or printing if the deck is not
           big enough. *)
        let hand, deck = Deck.draw_hand deck ~n:8 |> Or_error.ok_exn in
        deck, (connection, hand))
  in
  let%map players =
    Deferred.List.map
      ~how:(`Max_concurrent_jobs 16)
      connection_and_hands
      ~f:(fun (connection, hand) ->
        let%map name =
          Rpc.Rpc.dispatch Rpcs.Name.rpc connection () |> Deferred.Or_error.ok_exn
        in
        Player.{ connection; hand; name })
  in
  match List.permute players with
  | [] | [ _ ] ->
    Or_error.error_s [%message "More than 1 player is required to start the game"]
  | current_player :: hd :: tl ->
    Ongoing
      { deck = Deck.add_exploding_kittens deck ~n:(List.length players - 1)
      ; current_player
      ; other_players = Nonempty_list.create hd tl
      }
    |> Or_error.return
;;
