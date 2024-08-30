open! Core

(** The following type uses [String.Caseless] under the hood and hence
    is not case-sensitive. *)
type t [@@deriving bin_io, sexp]

include Comparable.S with type t := t

(* TODO-soon: We should also blacklist any keywords, like "you" or any card
   names. *)

(** An error is returned if the string is empty or contains blacklisted
    characters.

    See the [.ml] file for more details on blacklisted characters. *)
val of_string_or_error : string -> t Or_error.t

val of_string_exn : string -> t
val to_string : t -> string
