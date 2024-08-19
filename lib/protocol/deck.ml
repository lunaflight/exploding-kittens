open! Core
open! Async

type t = Card.t list [@@deriving bin_io, sexp]

let shuffle t = List.permute t

let init_with_counts ~count_of_card =
  Card.all
  |> List.map ~f:(fun card -> List.init (count_of_card card) ~f:(fun _i -> card))
  |> List.concat
;;

let default_without_exploding_kittens ~player_cnt =
  if player_cnt < 2 || player_cnt >= 5
  then
    Or_error.error_s
      [%message "This deck is suited for 2 to 5 players." (player_cnt : int)]
  else
    init_with_counts ~count_of_card:(function
      | Defuse -> if player_cnt <= 3 then 2 else 6 - player_cnt
      | Exploding_kitten -> 0
      | Power See_the_future -> 5
      | Power Skip -> 4
      | Powerless Beard_cat -> 4
      | Powerless Cattermelon -> 4
      | Powerless Hairy_potato_cat -> 4
      | Powerless Rainbow_ralphing_cat -> 4
      | Powerless Tacocat -> 4)
    |> shuffle
    |> Or_error.return
;;

let add_exploding_kittens t ~n =
  t @ List.init n ~f:(fun _i -> Card.Exploding_kitten) |> shuffle
;;

let draw_hand t ~n =
  if n <= 0 || n > List.length t
  then
    Or_error.error_s
      [%message
        "Attempting to draw invalid number of cards" (n : int) (List.length t : int)]
  else (
    let cards, deck = List.split_n t n in
    (Hand.of_cards cards, deck) |> Or_error.return)
;;

let draw t =
  match t with
  | [] -> Or_error.error_s [%message "Attempting to draw from an empty deck"]
  | hd :: tl -> Or_error.return (hd, tl)
;;

let peek t ~n = List.take t n

let insert t ~card ~position =
  let top, bottom =
    List.split_n t (if position >= 0 then position else List.length t + 1 + position)
  in
  top @ [ card ] @ bottom
;;

let size = List.length

module For_testing = struct
  let of_card_list t = t
end
