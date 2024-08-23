open! Core

(** The following type uses [String.Caseless] under the hood and hence
    is not case-sensitive. *)
type t [@@deriving bin_io, sexp]

include Stringable.S with type t := t
include Comparable.S with type t := t
