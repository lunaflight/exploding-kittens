open! Core
open! Async

module Power = struct
  type t =
    | See_the_future
    | Skip
    | Shuffle
  [@@deriving
    bin_io
    , compare
    , enumerate
    , sexp
    , string ~capitalize:"Title Case" ~case_insensitive]

  let of_string_exn = of_string

  let of_string_or_error string =
    Or_error.try_with (fun () -> of_string_exn string)
  ;;
end

module Powerless = struct
  type t =
    | Beard_cat
    | Cattermelon
    | Hairy_potato_cat
    | Rainbow_ralphing_cat [@rename "Rainbow-ralphing Cat"]
    | Tacocat
  [@@deriving
    bin_io
    , compare
    , enumerate
    , sexp
    , string ~capitalize:"Title Case" ~case_insensitive]
end

module T = struct
  type t =
    | Defuse
    | Exploding_kitten
    (* Ensure no string representations are shared during nesting. *)
    | Power of Power.t [@nested ""]
    | Powerless of Powerless.t [@nested ""]
  [@@deriving
    bin_io
    , compare
    , enumerate
    , sexp
    , string ~capitalize:"Title Case" ~case_insensitive]
end

(* TODO-someday: Avoid the indirection by using [include functor
   Comparator.Make] when it is upstreamed in the future. See:
   [https://github.com/ocaml-flambda/flambda-backend/blob/main/ocaml/jane/doc/extensions/include-functor.md] *)
include T
include Comparable.Make_binable (T)

let of_string_exn = of_string

let of_string_or_error string =
  Or_error.try_with (fun () -> of_string_exn string)
;;
