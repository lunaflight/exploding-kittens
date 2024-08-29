open! Core
include String.Caseless

let blacklisted_characters = [ '@' ]

let of_string_or_error string =
  if String.equal string ""
     || List.exists blacklisted_characters ~f:(fun char ->
       String.contains string char)
  then Or_error.error_s [%message "Invalid player name" (string : t)]
  else Ok string
;;

let of_string_exn string = of_string_or_error string |> Or_error.ok_exn
let to_string = Fn.id
