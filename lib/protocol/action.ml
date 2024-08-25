open! Core
open! Async

module Draw_or_play = struct
  type t =
    | Draw
    | Play of Card.Power.t [@nested ""]
  [@@deriving bin_io, enumerate, string ~capitalize:"Title Case" ~case_insensitive, sexp]

  let of_string t = Or_error.try_with (fun () -> of_string t)

  let handle t ~hand ~deck ~deterministically =
    match t with
    | Draw ->
      let%map.Or_error card, deck = Deck.draw deck in
      (match card with
       | Exploding_kitten ->
         (match Hand.remove_card hand ~card:Defuse with
          | Error _ -> Outcome.Exploded, Hand.add_card hand ~card, deck
          | Ok hand -> Outcome.Defused, hand, deck)
       | _ -> Outcome.Drew_safely card, Hand.add_card hand ~card, deck)
    | Play power ->
      let%map.Or_error hand = Hand.remove_card hand ~card:(Power power) in
      (match power with
       | See_the_future -> Outcome.Saw_the_future (Deck.peek deck ~n:3), hand, deck
       | Skip -> Outcome.Skipped, hand, deck
       | Shuffle -> Outcome.Shuffled, hand, Deck.shuffle deck ~deterministically)
  ;;
end

module Insert_exploding_kitten = struct
  let handle ~position ~deck =
    ( Outcome.Inserted_exploding_kitten position
    , Deck.insert deck ~card:Card.Exploding_kitten ~position )
  ;;
end
