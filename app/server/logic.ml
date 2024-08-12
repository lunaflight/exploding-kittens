open! Core
open! Async
open Protocol_lib

(* TODO: The player should have a name. This is so logs can be customised. *)
module Player = struct
  type t =
    { connection : Rpc.Connection.t
    ; hand : Card.t list
    }

  let get_action t =
    Rpc.Rpc.dispatch Rpcs.Get_action.rpc t.connection t.hand |> Deferred.Or_error.ok_exn
  ;;

  (* TODO: Sexps should be sent in a better fashion. This causes a lot of ugly
     [\n]s to be printed. Consider implementing [to_string] or [to_string_hum]
     for all things required to be sent. *)
  let send_message t sexp =
    Rpc.Rpc.dispatch Rpcs.Message.rpc t.connection (Sexplib.Sexp.to_string_hum sexp)
    |> Deferred.Or_error.ok_exn
  ;;
end

let rec gameplay_loop ~(players : Player.t list) ~deck =
  let update_hand_and_rotate_players ~(current_player : Player.t) ~rest_players ~hand =
    rest_players @ [ { current_player with hand } ]
  in
  match players with
  (* TODO: Use a nonempty list module. *)
  | [] -> raise_s [%message "There should be at least 1 player."]
  | [ player ] -> Player.send_message player [%message "You won!"]
  | current_player :: rest_players ->
    let%bind action = Player.get_action current_player in
    let outcome, hand, deck =
      Action.handle action ~hand:current_player.hand ~deck |> Or_error.ok_exn
    in
    (match outcome with
     | Action.Outcome.Drew_successfully ->
       let%bind () =
         Player.send_message
           current_player
           [%message "You drew successfully" (hand : Card.t list)]
       in
       gameplay_loop
         ~players:(update_hand_and_rotate_players ~current_player ~rest_players ~hand)
         ~deck
     | Action.Outcome.Exploded ->
       let%bind () = Player.send_message current_player [%message "You exploded!"] in
       gameplay_loop ~players:rest_players ~deck)
;;

let start ~connections =
  let deck, players =
    List.fold_map
      connections
      (* TODO: Provide a way to customise the starting deck and starting hand
         size. *)
      ~init:(Deck.default_without_exploding_kittens () |> Deck.shuffle)
      ~f:(fun deck connection ->
        let hand, deck = Deck.draw_cards deck ~n:8 in
        deck, Player.{ connection; hand })
  in
  gameplay_loop
    ~players
    ~deck:(Deck.add_exploding_kittens deck ~n:(List.length players - 1))
;;
