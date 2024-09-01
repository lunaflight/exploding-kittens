open! Core
open! Async

module Power : sig
  type t =
    | Attack
    | See_the_future
    | Shuffle
    | Skip
  [@@deriving bin_io, enumerate, sexp]

  val of_string_exn : string -> t
  val of_string_or_error : string -> t Or_error.t
  val to_string : t -> string
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
  | Defuse
  | Exploding_kitten
  | Power of Power.t
  | Powerless of Powerless.t
[@@deriving bin_io, compare, sexp]

include Comparable.S_binable with type t := t

val all : t list
val of_string_exn : string -> t
val of_string_or_error : string -> t Or_error.t
val to_string : t -> string
