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
[@@deriving bin_io, compare, sexp]

include Comparable.S_binable with type t := t

val all : t list
val to_string : t -> string
