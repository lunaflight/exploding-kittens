open! Core
open! Async
open Protocol_lib

let stdin = Lazy.force Reader.stdin
let stdout = Lazy.force Writer.stdout
let print_string = Writer.write_line stdout

let get_input ~prompt =
  Deferred.repeat_until_finished prompt (fun prompt ->
    Writer.write stdout prompt;
    match%map Reader.read_line stdin with
    | `Ok string -> `Finished string
    | `Eof ->
      Writer.newline stdout;
      Writer.write stdout "Interrupted; ";
      `Repeat prompt)
;;

let get ~prelude ~prompt ~input_hint ~of_string_or_error =
  Writer.write_line stdout prelude;
  Deferred.repeat_until_finished [%string "%{prompt} [%{input_hint}]: "] (fun prompt ->
    let%map input = get_input ~prompt in
    match of_string_or_error input with
    | Error _ ->
      Writer.write stdout "Couldn't parse; ";
      `Repeat prompt
    | Ok result -> `Finished result)
;;

let get_draw_or_play ~hand =
  (* TODO-soon: Analyze which actions are playable and output them. *)
  let possible_strings =
    Action.Draw_or_play.all
    |> List.map ~f:Action.Draw_or_play.to_string
    |> String.concat ~sep:"|"
  in
  get
    ~prelude:[%string "Your hand: %{hand#Hand}"]
    ~prompt:[%string "Provide an action"]
    ~input_hint:possible_strings
    ~of_string_or_error:Action.Draw_or_play.of_string
;;

(* TODO-soon: Provide a better interface than just a mysterous int as a postion. *)
let get_exploding_kitten_insert_position ~deck_size =
  let lowest_pos = -deck_size - 1 in
  let highest_pos = deck_size in
  get
    ~prelude:[%string "Deck size: %{deck_size#Int}"]
    ~prompt:[%string "Provide an insert position "]
    ~input_hint:[%string "%{lowest_pos#Int}..%{highest_pos#Int}"]
    ~of_string_or_error:(fun string ->
      Int.of_string_opt string
      |> Or_error.of_option
           ~error:(Error.create_s [%message "Could not parse as int" (string : string)]))
;;
