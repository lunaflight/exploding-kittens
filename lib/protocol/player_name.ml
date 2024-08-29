open! Core
include String.Caseless

(* TODO-soon: Ban empty names and certain characters like [@] used in action
   parsing. *)
let of_string = Fn.id
let to_string = Fn.id
