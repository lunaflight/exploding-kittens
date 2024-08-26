open! Core
open! Async

module Draw_or_play = struct
  type t =
    | Draw
    | Play of Card.Power.t [@nested ""]
  [@@deriving bin_io, enumerate, string ~capitalize:"Title Case" ~case_insensitive, sexp]

  let of_string t = Or_error.try_with (fun () -> of_string t)

  let handle t ~player_hands ~player_name ~deck ~deterministically =
    match t with
    | Draw ->
      let%bind.Or_error card, deck = Deck.draw deck in
      let%bind.Or_error player_hands_with_card_added =
        Player_hands.add_card player_hands ~player_name ~card
      in
      (match card with
       | Exploding_kitten ->
         (match%bind.Or_error
            Player_hands.has_card player_hands ~player_name ~card:Defuse
          with
          | false ->
            (Outcome.Exploded, player_hands_with_card_added, deck) |> Or_error.return
          | true ->
            let%map.Or_error player_hands =
              Player_hands.remove_card player_hands ~player_name ~card:Defuse
            in
            Outcome.Defused, player_hands, deck)
       | _ ->
         (Outcome.Drew_safely card, player_hands_with_card_added, deck) |> Or_error.return)
    | Play power ->
      let%map.Or_error player_hands =
        Player_hands.remove_card player_hands ~player_name ~card:(Power power)
      in
      (match power with
       | See_the_future ->
         Outcome.Saw_the_future (Deck.peek deck ~n:3), player_hands, deck
       | Skip -> Outcome.Skipped, player_hands, deck
       | Shuffle -> Outcome.Shuffled, player_hands, Deck.shuffle deck ~deterministically)
  ;;
end

module Insert_exploding_kitten = struct
  let handle ~position ~deck =
    ( Outcome.Inserted_exploding_kitten position
    , Deck.insert deck ~card:Card.Exploding_kitten ~position )
  ;;
end
