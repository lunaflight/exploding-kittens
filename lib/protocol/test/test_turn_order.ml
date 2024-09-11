open! Core
open Protocol_lib
module Player_and_turns = Turn_order.For_testing.Player_and_turns

let create_exn ~player_name_and_turns ~spectators =
  let spectators = List.map spectators ~f:Player_name.of_string_exn in
  match
    List.map player_name_and_turns ~f:(fun (player_name, turns) ->
      player_name
      |> Player_name.of_string_exn
      |> Player_and_turns.of_player_name ~turns)
  with
  | [] | [ _ ] -> raise_s [%message "player_names should be of length >= 2"]
  | current_player :: hd :: tl ->
    Turn_order.For_testing.create
      ~current_player
      ~waiting_players:(Nonempty_list.create hd tl)
      ~spectators
;;

let create_with_single_turns_exn ~player_names ~spectators =
  create_exn
    ~player_name_and_turns:
      (List.map player_names ~f:(fun player_name -> player_name, 1))
    ~spectators
;;

let create_and_print ~player_name_and_turns ~spectators =
  create_exn ~player_name_and_turns ~spectators
  |> Turn_order.to_string
  |> print_endline
;;

let%expect_test "string representation looks correct with 0 spectators" =
  create_and_print ~player_name_and_turns:[ "A", 1; "B", 1 ] ~spectators:[];
  [%expect {| Turn order: A -> B | Spectators: None |}]
;;

let%expect_test "string representation looks correct with 1 spectator" =
  create_and_print
    ~player_name_and_turns:[ "A", 1; "B", 2; "C", 3 ]
    ~spectators:[ "spectator" ];
  [%expect {| Turn order: A -> B -> C | Spectators: spectator |}]
;;

let%expect_test "string representation looks correct with > 1 spectator" =
  create_and_print
    ~player_name_and_turns:[ "A", 1; "B", 2; "C", 3 ]
    ~spectators:[ "spectator"; "spectator2"; "spectator3" ];
  [%expect
    {| Turn order: A -> B -> C | Spectators: spectator, spectator2, spectator3 |}]
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
    {|
    (turn_order
     (Ok
      ((current_player ((player_name A) (turns 1)))
       (waiting_players (((player_name B) (turns 1)))) (spectators ()))))
    |}]
;;

let%expect_test "3 players -> A, B, C in order and no spectators" =
  of_player_names_and_print ~first:"A" ~second:"B" ~others:[ "C" ];
  [%expect
    {|
    (turn_order
     (Ok
      ((current_player ((player_name A) (turns 1)))
       (waiting_players
        (((player_name B) (turns 1)) ((player_name C) (turns 1))))
       (spectators ()))))
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

let eliminate_current_player_and_print ~player_names ~spectators =
  let eliminated_outcome =
    create_with_single_turns_exn ~player_names ~spectators
    |> Turn_order.eliminate_current_player
  in
  print_s [%message (eliminated_outcome : Turn_order.Eliminated_outcome.t)]
;;

let%expect_test "after elimination with 2 players -> One_left returned with \
                 winner followed by spectators"
  =
  eliminate_current_player_and_print
    ~player_names:[ "A"; "B" ]
    ~spectators:[ "X"; "Y"; "Z" ];
  [%expect {| (eliminated_outcome (One_left (B (A X Y Z)))) |}]
;;

let%expect_test "after elimination with 3 players -> More_than_one_left \
                 returned with A in spectators"
  =
  eliminate_current_player_and_print
    ~player_names:[ "A"; "B"; "C" ]
    ~spectators:[ "X"; "Y"; "Z" ];
  [%expect
    {|
    (eliminated_outcome
     (More_than_one_left
      ((current_player ((player_name B) (turns 1)))
       (waiting_players (((player_name C) (turns 1)))) (spectators (A X Y Z)))))
    |}]
;;

let current_player_and_print ~player_names ~spectators =
  let current_player =
    create_with_single_turns_exn ~player_names ~spectators
    |> Turn_order.current_player
  in
  print_s [%message (current_player : Player_name.t)]
;;

let%expect_test "current player is the first in list" =
  current_player_and_print
    ~player_names:[ "A"; "B"; "C" ]
    ~spectators:[ "X"; "Y"; "Z" ];
  [%expect {| (current_player A) |}]
;;

let players_and_print ~player_names ~spectators ~players_of_turn_order =
  let result =
    create_with_single_turns_exn ~player_names ~spectators
    |> players_of_turn_order
  in
  print_s [%message (result : Player_name.t list)]
;;

let%expect_test "only spectators are output" =
  players_and_print
    ~player_names:[ "A"; "B"; "C" ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~players_of_turn_order:Turn_order.spectators;
  [%expect {| (result (X Y Z)) |}]
;;

let%expect_test "waiting players, B and C, are output" =
  players_and_print
    ~player_names:[ "A"; "B"; "C" ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~players_of_turn_order:Turn_order.waiting_players;
  [%expect {| (result (B C)) |}]
;;

let%expect_test "only waiting player C is output with B blacklisted" =
  players_and_print
    ~player_names:[ "A"; "B"; "C" ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~players_of_turn_order:
      (Turn_order.waiting_players_except
         ~blacklist:[ Player_name.of_string_exn "B" ]);
  [%expect {| (result (C)) |}]
;;

let%expect_test "players A, B, C are output" =
  players_and_print
    ~player_names:[ "A"; "B"; "C" ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~players_of_turn_order:Turn_order.players;
  [%expect {| (result (A B C)) |}]
;;

let pass_turn_and_print ~player_name_and_turns ~spectators =
  let turn_order =
    create_exn ~player_name_and_turns ~spectators |> Turn_order.pass_turn
  in
  print_s [%message (turn_order : Turn_order.t)]
;;

let%expect_test "pass turn with single turn -> turn order is rotated" =
  pass_turn_and_print
    ~player_name_and_turns:[ "A", 1; "B", 1; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ];
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 1)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "pass turn with 2 turns -> current player loses 1 turn" =
  pass_turn_and_print
    ~player_name_and_turns:[ "A", 2; "B", 1; "C", 2 ]
    ~spectators:[ "X"; "Y"; "Z" ];
  [%expect
    {|
    (turn_order
     ((current_player ((player_name A) (turns 1)))
      (waiting_players (((player_name B) (turns 1)) ((player_name C) (turns 2))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "pass turn with 5 turns -> current player loses 1 turn" =
  pass_turn_and_print
    ~player_name_and_turns:[ "A", 5; "B", 1; "C", 2 ]
    ~spectators:[ "X"; "Y"; "Z" ];
  [%expect
    {|
    (turn_order
     ((current_player ((player_name A) (turns 4)))
      (waiting_players (((player_name B) (turns 1)) ((player_name C) (turns 2))))
      (spectators (X Y Z))))
    |}]
;;

let give_all_turns_by_attack_and_print
  ~player_name_and_turns
  ~spectators
  ~additional_turns
  =
  let turn_order =
    create_exn ~player_name_and_turns ~spectators
    |> Turn_order.give_all_turns_by_attack ~additional_turns
  in
  print_s [%message (turn_order : Turn_order.t)]
;;

let%expect_test "give turn with 1 turn & 2 additional -> B has 2 moves" =
  give_all_turns_by_attack_and_print
    ~player_name_and_turns:[ "A", 1; "B", 1; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~additional_turns:2;
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 2)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "give turn with 1 turn & 2 additional & B has many moves -> B \
                 has 2 moves (overriden)"
  =
  give_all_turns_by_attack_and_print
    ~player_name_and_turns:[ "A", 1; "B", 10; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~additional_turns:2;
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 2)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "give turn with 2 turns & 2 additional -> B has 4 moves" =
  give_all_turns_by_attack_and_print
    ~player_name_and_turns:[ "A", 2; "B", 1; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~additional_turns:2;
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 4)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "give turn with 5 turns & 2 additional -> B has 7 moves" =
  give_all_turns_by_attack_and_print
    ~player_name_and_turns:[ "A", 5; "B", 1; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~additional_turns:2;
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 7)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "give turn with 1 turn & 1 additional -> B has 1 move" =
  give_all_turns_by_attack_and_print
    ~player_name_and_turns:[ "A", 1; "B", 1; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~additional_turns:1;
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 1)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "give turn with 2 turns & 1 additional -> B has 3 moves" =
  give_all_turns_by_attack_and_print
    ~player_name_and_turns:[ "A", 2; "B", 1; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~additional_turns:1;
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 3)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;

let%expect_test "give turn with 5 turns & 1 additional -> B has 6 moves" =
  give_all_turns_by_attack_and_print
    ~player_name_and_turns:[ "A", 5; "B", 1; "C", 1 ]
    ~spectators:[ "X"; "Y"; "Z" ]
    ~additional_turns:1;
  [%expect
    {|
    (turn_order
     ((current_player ((player_name B) (turns 6)))
      (waiting_players (((player_name C) (turns 1)) ((player_name A) (turns 1))))
      (spectators (X Y Z))))
    |}]
;;
