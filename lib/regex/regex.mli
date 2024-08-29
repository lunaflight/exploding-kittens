open! Core

(** Returns a list of all the capture groups that [string] matches against [regex].
    An error is returned if the [regex] is ill-formed.
    None is returned if [string] does not match [regex].
    Some [ capture_group_1; capture_group_2; ... ] is returned if it matches
    against [regex].

    For example, if [regex] = [test(s?)#(.)] and [string] = [test#A], then
    [[ ""; "A" ]] will be returned.*)
val capture_groups
  :  case_sensitive:bool
  -> regex:string
  -> string:string
  -> string list option Or_error.t

(** Equivalent to [capture_groups ... |> Or_error.ok_exn]. *)
val capture_groups_exn
  :  case_sensitive:bool
  -> regex:string
  -> string:string
  -> string list option
