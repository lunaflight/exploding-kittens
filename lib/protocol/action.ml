open! Core
open! Async

module Outcome = struct
  type t =
    | Defused
    | Drew_safely of Card.t
    | Exploded
    | Inserted_exploding_kitten of int
    | Saw_the_future of Card.t list
    | Skipped
  [@@deriving variants, sexp_of]

  let to_self_alert = function
    | Defused -> [%string "You defused an exploding kitten!"]
    (* TODO: Perhaps we can provide a lookup table to decide between "a" or
       "an" or download some library that handles this for us. *)
    | Drew_safely card -> [%string "You drew a(n) %{card#Card}."]
    | Inserted_exploding_kitten position ->
      [%string "You inserted an exploding kitten at position %{position#Int}."]
    | Exploded -> [%string "You exploded!"]
    | Saw_the_future cards ->
      let cards_string = List.map cards ~f:Card.to_string |> String.concat ~sep:", " in
      let n_cards =
        match List.length cards with
        | 1 -> "1 card"
        | cnt -> [%string "%{cnt#Int} cards"]
      in
      (match List.length cards with
       | 0 -> "You did not see any cards as the deck is empty."
       | _ -> [%string "You saw %{n_cards} at the top of the deck: %{cards_string}"])
    | Skipped -> [%string "You skipped your turn."]
  ;;

  let to_others_alert t ~name =
    match t with
    | Defused -> [%string "%{name} defused an exploding kitten!"]
    | Drew_safely _ -> [%string "%{name} drew a card."]
    | Inserted_exploding_kitten _ ->
      [%string "%{name} inserted an exploding kitten somewhere."]
    | Exploded -> [%string "%{name} exploded!"]
    | Saw_the_future cards ->
      let n_cards =
        match List.length cards with
        | 1 -> "1 card"
        | cnt -> [%string "%{cnt#Int} cards"]
      in
      [%string "%{name} saw the future of %{n_cards} at the top of the deck."]
    | Skipped -> [%string "%{name} skipped their turn."]
  ;;

  module For_testing = struct
    let all_mocked ~drew_safely ~inserted_exploding_kitten ~saw_the_future =
      Variants.fold
        ~init:[]
        ~defused:(fun acc v -> acc @ [ v.constructor ])
        ~drew_safely:(fun acc v ->
          List.map drew_safely ~f:v.constructor |> List.append acc)
        ~exploded:(fun acc v -> acc @ [ v.constructor ])
        ~inserted_exploding_kitten:(fun acc v ->
          List.map inserted_exploding_kitten ~f:v.constructor |> List.append acc)
        ~saw_the_future:(fun acc v ->
          List.map saw_the_future ~f:v.constructor |> List.append acc)
        ~skipped:(fun acc v -> acc @ [ v.constructor ])
    ;;
  end
end

module Next_step = struct
  type t =
    | Draw_or_play
    | Eliminate_player
    | Insert_exploding_kitten
    | Pass_turn
  [@@deriving sexp_of]

  let of_outcome =
    let open Outcome in
    function
    | Defused -> Insert_exploding_kitten
    | Drew_safely _ -> Pass_turn
    | Exploded -> Eliminate_player
    | Inserted_exploding_kitten _ -> Pass_turn
    | Saw_the_future _ -> Draw_or_play
    | Skipped -> Pass_turn
  ;;
end

module Draw_or_play = struct
  type t =
    | Draw
    | Play of Card.Power.t [@nested ""]
  [@@deriving bin_io, enumerate, string ~capitalize:"Title Case" ~case_insensitive, sexp]

  let of_string t = Or_error.try_with (fun () -> of_string t)

  let handle t ~hand ~deck =
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
       | Skip -> Outcome.Skipped, hand, deck
       | See_the_future -> Outcome.Saw_the_future (Deck.peek deck ~n:3), hand, deck)
  ;;
end

module Insert_exploding_kitten = struct
  let handle ~position ~deck =
    ( Outcome.Inserted_exploding_kitten position
    , Deck.insert deck ~card:Card.Exploding_kitten ~position )
  ;;
end
