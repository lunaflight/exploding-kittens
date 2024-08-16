open! Core
open! Async

module Outcome = struct
  type t =
    | Drew of Card.t
    | Exploded
    | Saw_the_future of Card.t list
    | Skipped
  [@@deriving variants, sexp_of]

  let to_self_alert = function
    (* TODO: Perhaps we can provide a lookup table to decide between "a" or
       "an" or download some library that handles this for us. *)
    | Drew card -> [%string "You drew a(n) %{card#Card}."]
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
    | Drew _ -> [%string "%{name} drew a card."]
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
    let all_mocked ~drew ~saw_the_future =
      Variants.fold
        ~init:[]
        ~drew:(fun acc v -> List.map drew ~f:v.constructor |> List.append acc)
        ~exploded:(fun acc v -> acc @ [ v.constructor ])
        ~saw_the_future:(fun acc v ->
          List.map saw_the_future ~f:v.constructor |> List.append acc)
        ~skipped:(fun acc v -> acc @ [ v.constructor ])
    ;;
  end
end

type t =
  | Draw
  | Play of Card.Power.t [@nested ""]
[@@deriving bin_io, enumerate, of_string ~case_insensitive, sexp]

let handle t ~hand ~deck =
  match t with
  | Draw ->
    let%map.Or_error card, deck = Deck.draw deck in
    (match card with
     | Exploding_kitten -> Outcome.Exploded, Hand.add_card hand ~card, deck
     | _ -> Outcome.Drew card, Hand.add_card hand ~card, deck)
  | Play power ->
    let%map.Or_error hand = Hand.remove_card hand ~card:(Power power) in
    (match power with
     | Skip -> Outcome.Skipped, hand, deck
     | See_the_future -> Outcome.Saw_the_future (Deck.peek deck ~n:3), hand, deck)
;;

let of_string t = Or_error.try_with (fun () -> of_string t)

module For_testing = struct
  let all = all
end
