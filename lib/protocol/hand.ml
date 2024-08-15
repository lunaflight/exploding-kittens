open! Core
open! Async

type t = int Card.Map.t [@@deriving bin_io, sexp]

let of_cards t =
  Card.Map.of_list_with_key_fold t ~get_key:Fun.id ~init:0 ~f:(fun acc _card -> acc + 1)
;;

let add_card t ~card =
  Map.update t card ~f:(function
    | None -> 1
    | Some count -> count + 1)
;;

let to_string t =
  Map.to_alist ~key_order:`Increasing t
  |> List.map ~f:(fun (card, count) -> [%string "%{count#Int}x %{card#Card}"])
  |> String.concat ~sep:", "
;;

let remove_card t ~card =
  match Map.find t card with
  | None -> Or_error.error_s [%message "Card is not owned" (card : Card.t) (t : t)]
  | Some count ->
    let new_count = count - 1 in
    (if new_count = 0 then Map.remove t card else Map.set t ~key:card ~data:new_count)
    |> Or_error.return
;;
