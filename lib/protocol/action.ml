open! Core
open! Async

module Outcome = struct
  type t =
    | Drew_successfully
    | Exploded
  [@@deriving sexp_of]
end

(* TODO: Add playing of cards *)
type t = Draw [@@deriving bin_io, sexp]

let handle t ~hand ~deck =
  match t with
  | Draw ->
    let%map.Or_error card, deck = Deck.draw deck in
    (match card with
     | Exploding_kitten -> Outcome.Exploded, card :: hand, deck
     | _ -> Outcome.Drew_successfully, card :: hand, deck)
;;
