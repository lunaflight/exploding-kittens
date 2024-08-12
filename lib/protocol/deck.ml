open! Core
open! Async

type t = Card.t list [@@deriving bin_io, sexp]

let shuffle t = List.permute t

let init_with_counts ~count_of_card =
  Card.all
  |> List.map ~f:(fun card -> List.init (count_of_card card) ~f:(fun _i -> card))
  |> List.concat
;;

let default_without_exploding_kittens () =
  init_with_counts ~count_of_card:(function
    | Exploding_kitten -> 0
    | Powerless Beard_cat -> 4
    | Powerless Cattermelon -> 4
    | Powerless Hairy_potato_cat -> 4
    | Powerless Rainbow_ralphing_cat -> 4
    | Powerless Tacocat -> 4)
  |> shuffle
;;

let add_exploding_kittens t ~n =
  t @ List.init n ~f:(fun _i -> Card.Exploding_kitten) |> shuffle
;;

let draw_cards t ~n = List.split_n t n

let draw t =
  match t with
  | [] -> Or_error.error_s [%message "Attempting to draw from an empty deck"]
  | hd :: tl -> Or_error.return (hd, tl)
;;

module For_testing = struct
  let of_card_list t = t
end
