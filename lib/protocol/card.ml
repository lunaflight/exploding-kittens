open! Core
open! Async

module Powerless = struct
  type t =
    | Beard_cat
    | Cattermelon
    | Hairy_potato_cat
    | Rainbow_ralphing_cat
    | Tacocat
  [@@deriving bin_io, enumerate, sexp]
end

(* TODO: Add power cards *)
type t =
  | Exploding_kitten
  | Powerless of Powerless.t
[@@deriving bin_io, enumerate, sexp]
