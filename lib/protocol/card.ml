open! Core
open! Async

module Powerless = struct
  type t =
    | Beard_cat
    | Cattermelon
    | Hairy_potato_cat
    | Rainbow_ralphing_cat [@rename "Rainbow-ralphing Cat"]
    | Tacocat
  [@@deriving bin_io, enumerate, sexp, string ~capitalize:"Title Case"]
end

(* TODO: Add power cards *)
type t =
  | Exploding_kitten
  (* Ensure no [Powerless.t] has the same string representation as [t]. *)
  | Powerless of Powerless.t [@nested ""]
[@@deriving bin_io, enumerate, sexp, string ~capitalize:"Title Case"]

let string_of_cards cards = List.map cards ~f:to_string |> String.concat ~sep:", "
