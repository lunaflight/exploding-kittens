open! Core

module Nonempty_list = struct
  include Nonempty_list

  (* This is equivalent to xs @ [ x ] without invoking [exn] functions. *)
  let append' xs x =
    List.rev xs |> Nonempty_list.create x |> Nonempty_list.reverse
  ;;
end

module Player_and_turns = struct
  type t =
    { player_name : Player_name.t
    ; turns : int
    }
  [@@deriving fields ~getters, sexp_of]

  let of_player_name player_name ~turns = { turns; player_name }
  let decrement_turn { player_name; turns } = { player_name; turns = turns - 1 }
end

type t =
  { current_player : Player_and_turns.t
  ; waiting_players : Player_and_turns.t Nonempty_list.t
  ; spectators : Player_name.t list
  }
[@@deriving fields, sexp_of]

let of_player_names ~first ~second ~others =
  match
    first :: second :: others |> List.contains_dup ~compare:Player_name.compare
  with
  | true ->
    Or_error.error_s
      [%message
        "Players must all be unique"
          (first : Player_name.t)
          (second : Player_name.t)
          (others : Player_name.t list)]
  | false ->
    Or_error.return
      { current_player = Player_and_turns.of_player_name first ~turns:1
      ; waiting_players =
          others
          |> List.map ~f:(Player_and_turns.of_player_name ~turns:1)
          |> Nonempty_list.create
               (Player_and_turns.of_player_name second ~turns:1)
      ; spectators = []
      }
;;

let current_player
  { current_player
  ; waiting_players = (_ : Player_and_turns.t Nonempty_list.t)
  ; spectators = (_ : Player_name.t list)
  }
  =
  Player_and_turns.player_name current_player
;;

let waiting_players
  { current_player = (_ : Player_and_turns.t)
  ; waiting_players
  ; spectators = (_ : Player_name.t list)
  }
  =
  waiting_players
  |> Nonempty_list.map ~f:Player_and_turns.player_name
  |> Nonempty_list.to_list
;;

let spectators
  { current_player = (_ : Player_and_turns.t)
  ; waiting_players = (_ : Player_and_turns.t Nonempty_list.t)
  ; spectators
  }
  =
  spectators
;;

let waiting_players_except
  { current_player = (_ : Player_and_turns.t)
  ; waiting_players
  ; spectators = (_ : Player_name.t list)
  }
  ~blacklist
  =
  waiting_players
  |> Nonempty_list.map ~f:Player_and_turns.player_name
  |> Nonempty_list.filter ~f:(fun player_name ->
    List.mem blacklist player_name ~equal:Player_name.equal |> not)
;;

let players
  { current_player; waiting_players; spectators = (_ : Player_name.t list) }
  =
  Nonempty_list.cons current_player waiting_players
  |> Nonempty_list.map ~f:Player_and_turns.player_name
  |> Nonempty_list.to_list
;;

let pass_turn { current_player; waiting_players; spectators } =
  if Player_and_turns.turns current_player <= 1
  then (
    let (second_player :: tl) = waiting_players in
    { current_player = second_player
    ; waiting_players = Nonempty_list.append' tl current_player
    ; spectators
    })
  else
    { current_player = Player_and_turns.decrement_turn current_player
    ; waiting_players
    ; spectators
    }
;;

let give_all_turns_by_attack
  { current_player; waiting_players; spectators }
  ~additional_turns
  =
  let%tydi { player_name = first_player; turns } = current_player in
  let ({ player_name = second_player; turns = (_ : int) } :: tl) =
    waiting_players
  in
  { current_player =
      { player_name = second_player
      ; turns =
          (if turns = 1 then additional_turns else turns + additional_turns)
      }
  ; waiting_players =
      Nonempty_list.append' tl { player_name = first_player; turns = 1 }
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
  let spectators = Player_and_turns.player_name current_player :: spectators in
  match waiting_players with
  | [ { player_name; turns = (_ : int) } ] ->
    Eliminated_outcome.One_left (player_name, spectators)
  | new_current_player :: hd :: tl ->
    Eliminated_outcome.More_than_one_left
      { current_player = new_current_player
      ; waiting_players = Nonempty_list.create hd tl
      ; spectators
      }
;;

module For_testing = struct
  module Player_and_turns = Player_and_turns

  let create = Fields.create
end
