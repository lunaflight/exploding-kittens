open! Core
open! Async

module Outcome = struct
  type t =
    | Drew_successfully of Card.t
    | Exploded
  [@@deriving sexp_of]
end

(* TODO: Add playing of cards *)
type t = Draw [@@deriving bin_io, sexp, string ~case_insensitive]

let handle t ~hand ~deck =
  match t with
  | Draw ->
    let%map.Or_error card, deck = Deck.draw deck in
    (match card with
     | Exploding_kitten -> Outcome.Exploded, card :: hand, deck
     | _ -> Outcome.Drew_successfully card, card :: hand, deck)
;;

let of_string t = Or_error.try_with (fun () -> of_string t)
