open! Core
open! Async

type t = int Card.Map.t [@@deriving bin_io, sexp]

let of_cards t =
  Card.Map.of_list_with_key_fold t ~get_key:Fun.id ~init:0 ~f:(fun acc _card ->
    acc + 1)
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

let remove_card t ~card ~n =
  match Map.find t card with
  | None ->
    Or_error.error_s [%message "Card is not owned" (t : t) (card : Card.t)]
  | Some count ->
    if count < n
    then
      Or_error.error_s
        [%message "Not enough copies owned" (t : t) (card : Card.t) (n : int)]
    else (
      let new_count = count - n in
      (if new_count = 0
       then Map.remove t card
       else Map.set t ~key:card ~data:new_count)
      |> Or_error.return)
;;

let contains t ~card = Map.find t card |> Option.is_some

let random_card t ~deterministically =
  (* TODO-someday: There is a better algorithm that takes O(|Map.keys t|)
     time. *)
  t
  |> Map.to_alist
  |> List.concat_map ~f:(fun (card, count) ->
    List.init count ~f:(fun _i -> card))
  |> List.random_element
       ?random_state:
         (if deterministically then Random.State.make [||] |> Some else None)
;;
