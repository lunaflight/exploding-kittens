open! Core
open Protocol_lib

let create_exn ~player_names ~spectators =
  let spectators = List.map spectators ~f:Player_name.of_string_exn in
  match List.map player_names ~f:Player_name.of_string_exn with
  | [] | [ _ ] -> raise_s [%message "player_names should be of length >= 2"]
  | current_player :: hd :: tl ->
    Turn_order.For_testing.create
      ~current_player
      ~waiting_players:(Nonempty_list.create hd tl)
      ~spectators
;;

let of_player_names_and_print ~first ~second ~others =
  let turn_order =
    Turn_order.of_player_names
      ~first:(Player_name.of_string_exn first)
      ~second:(Player_name.of_string_exn second)
      ~others:(List.map others ~f:Player_name.of_string_exn)
  in
  print_s [%message (turn_order : Turn_order.t Or_error.t)]
;;

let%expect_test "2 players -> A, B in order and no spectators" =
  of_player_names_and_print ~first:"A" ~second:"B" ~others:[];
  [%expect
    {| (turn_order (Ok ((current_player A) (waiting_players (B)) (spectators ())))) |}]
;;

let%expect_test "3 players -> A, B, C in order and no spectators" =
  of_player_names_and_print ~first:"A" ~second:"B" ~others:[ "C" ];
  [%expect
    {|
    (turn_order
     (Ok ((current_player A) (waiting_players (B C)) (spectators ()))))
    |}]
;;

let%expect_test "duplicate players in first and second -> error" =
  of_player_names_and_print ~first:"A" ~second:"A" ~others:[];
  [%expect
    {|
    (turn_order
     (Error ("Players must all be unique" (first A) (second A) (others ()))))
    |}]
;;

let%expect_test "duplicate players in first and waitings -> error" =
  of_player_names_and_print ~first:"A" ~second:"B" ~others:[ "C"; "A" ];
  [%expect
    {|
    (turn_order
     (Error ("Players must all be unique" (first A) (second B) (others (C A)))))
    |}]
;;

let%expect_test "duplicate players in second and waitings -> error" =
  of_player_names_and_print ~first:"A" ~second:"B" ~others:[ "C"; "B" ];
  [%expect
    {|
    (turn_order
     (Error ("Players must all be unique" (first A) (second B) (others (C B)))))
    |}]
;;

let%expect_test "after elimination with 2 players -> One_left returned with winner \
                 followed by spectators"
  =
  let eliminated_outcome =
    create_exn ~player_names:[ "A"; "B" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.eliminate_current_player
  in
  print_s [%message (eliminated_outcome : Turn_order.Eliminated_outcome.t)];
  [%expect {| (eliminated_outcome (One_left (B (A X Y Z)))) |}]
;;

let%expect_test "after elimination with 3 players -> More_than_one_left returned with A \
                 in spectators"
  =
  let eliminated_outcome =
    create_exn ~player_names:[ "A"; "B"; "C" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.eliminate_current_player
  in
  print_s [%message (eliminated_outcome : Turn_order.Eliminated_outcome.t)];
  [%expect
    {|
    (eliminated_outcome
     (More_than_one_left
      ((current_player B) (waiting_players (C)) (spectators (A X Y Z)))))
    |}]
;;

let%expect_test "pass turn -> turn order is rotated" =
  let turn_order =
    create_exn ~player_names:[ "A"; "B"; "C" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.pass_turn
  in
  print_s [%message (turn_order : Turn_order.t)];
  [%expect
    {|
    (turn_order
     ((current_player B) (waiting_players (C A)) (spectators (X Y Z))))
    |}]
;;

let%expect_test "current player is the first in list" =
  let current_player =
    create_exn ~player_names:[ "A"; "B"; "C" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.current_player
  in
  print_s [%message (current_player : Player_name.t)];
  [%expect {| (current_player A) |}]
;;

let%expect_test "current player, A, and spectators are output" =
  let current_and_spectators =
    create_exn ~player_names:[ "A"; "B"; "C" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.current_and_spectators
  in
  print_s [%message (current_and_spectators : Player_name.t list)];
  [%expect {| (current_and_spectators (A X Y Z)) |}]
;;

let%expect_test "waiting players, B and C, are output" =
  let waiting_players =
    create_exn ~player_names:[ "A"; "B"; "C" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.waiting_players
  in
  print_s [%message (waiting_players : Player_name.t list)];
  [%expect {| (waiting_players (B C)) |}]
;;

let%expect_test "only C is output" =
  let waiting_players =
    create_exn ~player_names:[ "A"; "B"; "C" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.waiting_players_except ~blacklist:[ Player_name.of_string_exn "B" ]
  in
  print_s [%message (waiting_players : Player_name.t list)];
  [%expect {| (waiting_players (C)) |}]
;;

let%expect_test "players A, B, C are output" =
  let players =
    create_exn ~player_names:[ "A"; "B"; "C" ] ~spectators:[ "X"; "Y"; "Z" ]
    |> Turn_order.players
  in
  print_s [%message (players : Player_name.t list)];
  [%expect {| (players (A B C)) |}]
;;
