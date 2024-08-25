open! Core

(** This is an abstraction over a turn order of players. It also remembers
    spectators which includes eliminated players.
    It is asserted at compile-time that there is at least 2 players playing. *)
type t [@@deriving sexp_of]

(** Initialises [t] with the given [Player_name.t]s. All players start off
    playing - none of them are spectators.
    The turn order is as follows: [first] goes first, followed by [second],
    followed by [others] in the order of the given list. The head of the list
    takes lead.

    An error is returned if there are duplicate [Player_name.t]s. *)
val of_player_names
  :  first:Player_name.t
  -> second:Player_name.t
  -> others:Player_name.t list
  -> t Or_error.t

val current_player : t -> Player_name.t
val current_and_spectators : t -> Player_name.t list
val players : t -> Player_name.t list

(** Returns [players t] without [current_player t]. *)
val waiting_players : t -> Player_name.t list

(** Returns [waiting_players] without any player in [blacklist]. *)
val waiting_players_except : t -> blacklist:Player_name.t list -> Player_name.t list

(** Moves the [current_player] to the end of the queue, promoting the next
    waiting player to be the [current_player]. *)
val pass_turn : t -> t

module Eliminated_outcome : sig
  type nonrec t =
    | One_left of (Player_name.t * Player_name.t list)
    | More_than_one_left of t
  [@@deriving sexp_of]
end

(** Eliminates the current player.

    If there is only 1 player remaining, it returns [Winner] of the winner
    followed by all eliminated players and spectators.
    If there is still more than 1 player remaining, it returns [Continue] of
    [t] with the current player turned into a spectator. *)
val eliminate_current_player : t -> Eliminated_outcome.t

module For_testing : sig
  val create
    :  current_player:Player_name.t
    -> waiting_players:Player_name.t Nonempty_list.t
    -> spectators:Player_name.t list
    -> t
end
