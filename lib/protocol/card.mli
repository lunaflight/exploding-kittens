open! Core
open! Async

module Powerless : sig
  type t =
    | Beard_cat
    | Cattermelon
    | Hairy_potato_cat
    | Rainbow_ralphing_cat
    | Tacocat
end

type t =
  | Exploding_kitten
  | Powerless of Powerless.t
[@@deriving sexp, bin_io]

val all : t list
val to_string : t -> string
val string_of_cards : t list -> string
