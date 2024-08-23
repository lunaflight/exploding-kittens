open! Core
open! Async

type t = Hand.t Player_name.Map.t [@@deriving sexp_of]

let init ~player_names ~deck ~cards_per_player ~deterministically =
  let deck, player_name_and_hands =
    List.fold_map
      player_names
      ~init:(Deck.Without_exploding_kittens.shuffle deck ~deterministically)
      ~f:(fun deck name ->
        match Deck.Without_exploding_kittens.deal deck ~n:cards_per_player with
        | Ok (hand, deck) ->
          deck, (name, Hand.add_card ~card:Defuse hand) |> Or_error.return
        | Error _ as err -> deck, Or_error.tag err ~tag:"Deck is not sufficiently large")
  in
  let%bind.Or_error player_name_and_hands = Or_error.all player_name_and_hands in
  let%bind.Or_error t = Player_name.Map.of_alist_or_error player_name_and_hands in
  let%map.Or_error deck =
    Deck.add_exploding_kittens
      deck
      ~player_cnt:(List.length player_names)
      ~deterministically
    |> Or_error.tag ~tag:"Could not add exploding kittens"
  in
  deck, t
;;

let hand t ~player_name = Map.find_or_error t player_name
let hand_exn t ~player_name = hand t ~player_name |> Or_error.ok_exn

let set_hand t ~player_name ~hand =
  let%map.Or_error _hand = Map.find_or_error t player_name in
  Map.set t ~key:player_name ~data:hand
;;

let set_hand_exn t ~player_name ~hand = set_hand t ~player_name ~hand |> Or_error.ok_exn

let add_card t ~player_name ~card =
  let%map.Or_error hand = hand t ~player_name in
  Map.set t ~key:player_name ~data:(Hand.add_card hand ~card)
;;

let remove_card t ~player_name ~card =
  let%bind.Or_error hand = hand t ~player_name in
  let%map.Or_error hand = Hand.remove_card hand ~card in
  Map.set t ~key:player_name ~data:hand
;;

module For_testing = struct
  let of_alist_exn = Player_name.Map.of_alist_exn
end
