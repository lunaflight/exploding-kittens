open! Core
open! Async
open Protocol_lib

(* TODO: The player should have a name. This is so logs can be customised. *)
module Player = struct
  type t =
    { connection : Rpc.Connection.t
    ; hand : Hand.t
    }

  let get_action t =
    Rpc.Rpc.dispatch Rpcs.Get_action.rpc t.connection t.hand |> Deferred.Or_error.ok_exn
  ;;

  let send_message t message =
    Rpc.Rpc.dispatch Rpcs.Message.rpc t.connection message |> Deferred.Or_error.ok_exn
  ;;
end

let rec get_action player ~deck ~prompt =
  let%bind () = Player.send_message player prompt in
  let%bind action = Player.get_action player in
  match Action.handle action ~hand:player.hand ~deck with
  | Error _ -> get_action player ~deck ~prompt:"Cannot perform action, try again."
  | Ok result -> return result
;;

let rec gameplay_loop ~(players : Player.t list) ~deck =
  match players with
  (* TODO: Use a nonempty list module. *)
  | [] -> raise_s [%message "There should be at least 1 player."]
  | [ player ] -> Player.send_message player "You won!"
  | current_player :: rest_players ->
    let%bind outcome, hand, deck =
      get_action current_player ~deck ~prompt:"Input action:"
    in
    let current_player = { current_player with hand } in
    (match outcome with
     | Action.Outcome.Drew card ->
       let%bind () =
         Player.send_message current_player [%string "You drew a %{card#Card}."]
       in
       gameplay_loop ~players:(rest_players @ [ current_player ]) ~deck
     | Action.Outcome.Exploded ->
       let%bind () = Player.send_message current_player "You exploded!" in
       gameplay_loop ~players:rest_players ~deck
     | Action.Outcome.Played Skip ->
       gameplay_loop ~players:(rest_players @ [ current_player ]) ~deck)
;;

let start ~connections =
  let deck, players =
    List.fold_map
      connections
      (* TODO: Provide a way to customise the starting deck and starting hand
         size. *)
      ~init:(Deck.default_without_exploding_kittens () |> Deck.shuffle)
      ~f:(fun deck connection ->
        (* TODO: Properly handle re-prompting or printing if the deck is not
           big enough. *)
        let hand, deck = Deck.draw_hand deck ~n:8 |> Or_error.ok_exn in
        deck, Player.{ connection; hand })
  in
  gameplay_loop
    ~players
    ~deck:(Deck.add_exploding_kittens deck ~n:(List.length players - 1))
;;
