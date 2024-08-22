open! Core
open! Async
open Protocol_lib

type t =
  { connection : Rpc.Connection.t
  ; hand : Hand.t
  ; name : string
  }

let players_of_connections connections ~deck ~cards_per_player =
  let open Deferred.Or_error.Let_syntax in
  let deck, connection_and_hands =
    List.fold_map connections ~init:deck ~f:(fun deck connection ->
      match Deck.Without_exploding_kittens.deal deck ~n:cards_per_player with
      | Ok (hand, deck) ->
        deck, (connection, Hand.add_card ~card:Defuse hand) |> Or_error.return
      | Error _ as err -> deck, Or_error.tag err ~tag:"Deck is not sufficiently large")
  in
  let%bind connection_and_hands = Or_error.all connection_and_hands |> Deferred.return in
  let%map players =
    Monitor.try_with_or_error (fun () ->
      Deferred.List.map
        ~how:(`Max_concurrent_jobs 16)
        connection_and_hands
        ~f:(fun (connection, hand) ->
          let%map.Deferred name =
            Rpc.Rpc.dispatch Rpcs.Name.rpc connection () |> Deferred.Or_error.ok_exn
          in
          { connection; hand; name }))
  in
  deck, players
;;

let get_draw_or_play { connection; hand; name = (_ : string) } =
  Rpc.Rpc.dispatch Rpcs.Get_draw_or_play.rpc connection hand |> Deferred.Or_error.ok_exn
;;

let get_exploding_kitten_insert_position
  { connection; hand = (_ : Hand.t); name = (_ : string) }
  ~deck_size
  =
  Rpc.Rpc.dispatch Rpcs.Get_exploding_kitten_insert_position.rpc connection deck_size
  |> Deferred.Or_error.ok_exn
;;

let send_message { connection; hand = (_ : Hand.t); name = (_ : string) } message =
  Rpc.Rpc.dispatch Rpcs.Message.rpc connection message |> Deferred.Or_error.ok_exn
;;
