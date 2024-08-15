open! Core
open! Async

module Outcome = struct
  type t =
    | Drew of Card.t
    | Exploded
    | Played of Card.Power.t
  [@@deriving sexp_of]
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
  | Play Skip ->
    let%map.Or_error hand = Hand.remove_card hand ~card:(Power Skip) in
    Outcome.Played Skip, hand, deck
;;

let of_string t = Or_error.try_with (fun () -> of_string t)

module For_testing = struct
  let all = all
end
