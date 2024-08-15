open! Core
open! Async

module Power : sig
  type t = Skip [@@deriving bin_io, enumerate, of_string, sexp]
end

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
  | Power of Power.t
  | Powerless of Powerless.t
[@@deriving bin_io, compare, sexp]

include Comparable.S_binable with type t := t

val all : t list
val to_string : t -> string
