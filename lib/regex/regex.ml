open! Core

let capture_groups ~case_sensitive ~regex ~string =
  let%map.Or_error re =
    Re2.create ~options:{ Re2.Options.default with case_sensitive } [%string "^%{regex}$"]
  in
  match Re2.find_submatches re string with
  | Error _error -> None
  | Ok array ->
    let list = List.of_array array in
    (match list with
     | [] -> None
     | _full_match :: tl -> tl |> Option.all)
;;

let capture_groups_exn ~case_sensitive ~regex ~string =
  capture_groups ~case_sensitive ~regex ~string |> Or_error.ok_exn
;;
