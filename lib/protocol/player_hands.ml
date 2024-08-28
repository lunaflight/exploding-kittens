open! Core
open! Async

module Hand_or_eliminated = struct
  type t =
    | Eliminated
    | Playing of Hand.t
  [@@deriving sexp_of]
end

type t = Hand_or_eliminated.t Player_name.Map.t [@@deriving sexp_of]

let init ~player_names ~deck ~cards_per_player ~deterministically =
  let deck, player_name_and_hands =
    List.fold_map
      player_names
      ~init:(Deck.Without_exploding_kittens.shuffle deck ~deterministically)
      ~f:(fun deck name ->
        match Deck.Without_exploding_kittens.deal deck ~n:cards_per_player with
        | Ok (hand, deck) ->
          ( deck
          , (name, Hand_or_eliminated.Playing (Hand.add_card ~card:Defuse hand))
            |> Or_error.return )
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

let find_or_error t ~player_name =
  Map.find_or_error t player_name
  |> Or_error.tag_s
       ~tag:[%message "Could not find player name" (player_name : Player_name.t) (t : t)]
;;

let hand_or_error t ~player_name =
  match%bind.Or_error find_or_error t ~player_name with
  | Eliminated ->
    Or_error.error_s [%message "Player is eliminated" (player_name : Player_name.t)]
  | Playing hand -> Or_error.return hand
;;

let hand_exn t ~player_name = hand_or_error t ~player_name |> Or_error.ok_exn

let set_hand t ~player_name ~hand =
  let%map.Or_error _hand = hand_or_error t ~player_name in
  Map.set t ~key:player_name ~data:(Playing hand)
;;

let set_hand_exn t ~player_name ~hand = set_hand t ~player_name ~hand |> Or_error.ok_exn

let add_card t ~player_name ~card =
  let%map.Or_error hand = hand_or_error t ~player_name in
  Map.set t ~key:player_name ~data:(Hand.add_card hand ~card |> Playing)
;;

let remove_card t ~player_name ~card ~n =
  let%bind.Or_error hand = hand_or_error t ~player_name in
  let%map.Or_error hand = Hand.remove_card hand ~card ~n in
  Map.set t ~key:player_name ~data:(Playing hand)
;;

let has_card t ~player_name ~card =
  let%map.Or_error hand = hand_or_error t ~player_name in
  Hand.contains hand ~card
;;

let transfer_random_card t ~receiver ~target ~deterministically =
  let%bind.Or_error target_hand = hand_or_error t ~player_name:target in
  let%bind.Or_error card =
    Hand.random_card target_hand ~deterministically
    |> Or_error.of_option
         ~error:
           (Error.create_s [%message "Target has an empty hand" (target : Player_name.t)])
  in
  let%bind.Or_error t = remove_card t ~player_name:target ~card ~n:1 in
  let%map.Or_error t = add_card t ~player_name:receiver ~card in
  card, t
;;

let eliminate t ~player_name =
  let%map.Or_error _hand_or_eliminated = find_or_error t ~player_name in
  Map.set t ~key:player_name ~data:Eliminated
;;

let to_playing_alist t =
  Map.to_alist t
  |> List.filter_map ~f:(fun (player_name, hand_or_eliminated) ->
    match hand_or_eliminated with
    | Hand_or_eliminated.Playing hand -> Some (player_name, hand)
    | Hand_or_eliminated.Eliminated -> None)
;;

module For_testing = struct
  module Hand_or_eliminated = Hand_or_eliminated

  let of_alist_exn = Player_name.Map.of_alist_exn
end
