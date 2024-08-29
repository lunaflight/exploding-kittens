open! Core

(** TODO-soon: When the [Attack] card is added, consider using an assoc list
    to track turns left for a player to make. It is also likely that ensuring
    that each player only appears once in the order is important. *)
type t =
  { current_player : Player_name.t
  ; waiting_players : Player_name.t Nonempty_list.t
  ; spectators : Player_name.t list
  }
[@@deriving fields, sexp_of]

let of_player_names ~first ~second ~others =
  match first :: second :: others |> List.contains_dup ~compare:Player_name.compare with
  | true ->
    Or_error.error_s
      [%message
        "Players must all be unique"
          (first : Player_name.t)
          (second : Player_name.t)
          (others : Player_name.t list)]
  | false ->
    Or_error.return
      { current_player = first
      ; waiting_players = Nonempty_list.create second others
      ; spectators = []
      }
;;

let current_player
  { current_player
  ; waiting_players = (_ : Player_name.t Nonempty_list.t)
  ; spectators = (_ : Player_name.t list)
  }
  =
  current_player
;;

let waiting_players
  { current_player = (_ : Player_name.t)
  ; waiting_players
  ; spectators = (_ : Player_name.t list)
  }
  =
  Nonempty_list.to_list waiting_players
;;

let spectators
  { current_player = (_ : Player_name.t)
  ; waiting_players = (_ : Player_name.t Nonempty_list.t)
  ; spectators
  }
  =
  spectators
;;

let waiting_players_except
  { current_player = (_ : Player_name.t)
  ; waiting_players
  ; spectators = (_ : Player_name.t list)
  }
  ~blacklist
  =
  Nonempty_list.filter waiting_players ~f:(fun player_name ->
    List.mem blacklist player_name ~equal:Player_name.equal |> not)
;;

let players { current_player; waiting_players; spectators = (_ : Player_name.t list) } =
  Nonempty_list.cons current_player waiting_players |> Nonempty_list.to_list
;;

let pass_turn { current_player; waiting_players; spectators } =
  let (second_player :: tl) = waiting_players in
  { current_player = second_player
  ; waiting_players =
      List.rev tl |> Nonempty_list.create current_player |> Nonempty_list.reverse
      (* The above is equivalent to tl @ [ current_player ] without
     invoking [exn] functions. *)
  ; spectators
  }
;;

module Eliminated_outcome = struct
  type nonrec t =
    | One_left of (Player_name.t * Player_name.t list)
    | More_than_one_left of t
  [@@deriving sexp_of]
end

let eliminate_current_player { current_player; waiting_players; spectators } =
  let spectators = current_player :: spectators in
  match waiting_players with
  | [ player ] -> Eliminated_outcome.One_left (player, spectators)
  | new_current_player :: hd :: tl ->
    Eliminated_outcome.More_than_one_left
      { current_player = new_current_player
      ; waiting_players = Nonempty_list.create hd tl
      ; spectators
      }
;;

module For_testing = struct
  let create = Fields.create
end
