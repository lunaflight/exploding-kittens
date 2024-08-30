open! Core

(** The following function should be used during [Variants.fold ~init:[]] and
    accumulating on a variant that takes no arguments. *)
val accumulate_without_args : 'a list -> 'a Variant.t -> 'a list

(** The following function should be used during [Variants.fold ~init:[]] and
    accumulating on a variant that takes 1 argument, supplied with [args]. *)
val accumulate_with_args
  :  'a list
  -> ('b -> 'a) Variant.t
  -> args:'b list
  -> 'a list
