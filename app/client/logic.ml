open! Core
open! Async
open Protocol_lib

let stdin = Lazy.force Reader.stdin
let stdout = Lazy.force Writer.stdout
let print_string = Writer.write_line stdout

let rec get_input ~prompt =
  print_string prompt;
  match%bind Reader.read_line stdin with
  | `Ok string -> return string
  (* TODO: EOF may be a good indicator that the client connection
     should close instead. *)
  | `Eof -> get_input ~prompt:"Unexpected EOF: Try again."
;;

let get_action ~hand =
  let rec make_action' ~prompt =
    let%bind input = get_input ~prompt in
    match Action.of_string input with
    | Ok action -> return action
    | Error _ -> make_action' ~prompt:"Unknown action: Try again."
  in
  make_action' ~prompt:[%string "Your hand: %{Card.string_of_cards hand}"]
;;
