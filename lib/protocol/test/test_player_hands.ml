open! Core
open Protocol_lib
open Player_hands.For_testing.Hand_or_eliminated

let%test_module "init" =
  (module struct
    let init_and_print ~names ~deck ~cards_per_player =
      match
        Player_hands.init
          ~player_names:(List.map names ~f:Player_name.of_string)
          ~deck
          ~cards_per_player
          ~deterministically:true
      with
      | Error error -> print_s [%message (error : Error.t)]
      | Ok (deck, player_hands) ->
        print_s [%message (deck : Deck.t) (player_hands : Player_hands.t)]
    ;;

    let%expect_test "each player has 2 random cards and a defuse, deck has 1 exploding \
                     kitten"
      =
      init_and_print
        ~names:[ "A"; "B" ]
        ~deck:
          (Deck.Without_exploding_kittens.For_testing.of_card_list
             [ Card.Powerless Beard_cat
             ; Card.Powerless Cattermelon
             ; Card.Powerless Hairy_potato_cat
             ; Card.Powerless Rainbow_ralphing_cat
             ; Card.Powerless Tacocat
             ])
        ~cards_per_player:2;
      [%expect
        {|
        ((deck (Exploding_kitten (Powerless Beard_cat)))
         (player_hands
          ((A
            (Playing
             ((Defuse 1) ((Powerless Rainbow_ralphing_cat) 1)
              ((Powerless Tacocat) 1))))
           (B
            (Playing
             ((Defuse 1) ((Powerless Cattermelon) 1)
              ((Powerless Hairy_potato_cat) 1)))))))
        |}]
    ;;

    let%expect_test "0 players -> error" =
      init_and_print
        ~names:[]
        ~deck:
          (Deck.Without_exploding_kittens.For_testing.of_card_list
             [ Card.Powerless Beard_cat
             ; Card.Powerless Cattermelon
             ; Card.Powerless Hairy_potato_cat
             ; Card.Powerless Rainbow_ralphing_cat
             ; Card.Powerless Tacocat
             ])
        ~cards_per_player:2;
      [%expect
        {|
    (error
     ("Could not add exploding kittens"
      ("There should be at least 1 player" (player_cnt 0))))
    |}]
    ;;

    let%expect_test "1 player -> player draws random cards, deck has no exploding kittens"
      =
      init_and_print
        ~names:[ "A" ]
        ~deck:
          (Deck.Without_exploding_kittens.For_testing.of_card_list
             [ Card.Powerless Beard_cat
             ; Card.Powerless Cattermelon
             ; Card.Powerless Hairy_potato_cat
             ; Card.Powerless Rainbow_ralphing_cat
             ; Card.Powerless Tacocat
             ])
        ~cards_per_player:2;
      [%expect
        {|
        ((deck
          ((Powerless Hairy_potato_cat) (Powerless Cattermelon)
           (Powerless Beard_cat)))
         (player_hands
          ((A
            (Playing
             ((Defuse 1) ((Powerless Rainbow_ralphing_cat) 1)
              ((Powerless Tacocat) 1)))))))
        |}]
    ;;

    let%expect_test "deck is insufficiently big -> error" =
      init_and_print
        ~names:[ "A"; "B" ]
        ~deck:
          (Deck.Without_exploding_kittens.For_testing.of_card_list
             [ Card.Powerless Beard_cat
             ; Card.Powerless Cattermelon
             ; Card.Powerless Hairy_potato_cat
             ; Card.Powerless Rainbow_ralphing_cat
             ; Card.Powerless Tacocat
             ])
        ~cards_per_player:3;
      [%expect
        {|
    (error
     ("Deck is not sufficiently large"
      ("Attempting to draw invalid number of cards" (n 3) ("List.length t" 2))))
    |}]
    ;;

    let%expect_test "cards_per_player is negative -> error" =
      init_and_print
        ~names:[ "A"; "B" ]
        ~deck:(Deck.Without_exploding_kittens.For_testing.of_card_list [])
        ~cards_per_player:(-1);
      [%expect
        {|
        (error
         (("Deck is not sufficiently large"
           ("Attempting to draw invalid number of cards" (n -1) ("List.length t" 0)))
          ("Deck is not sufficiently large"
           ("Attempting to draw invalid number of cards" (n -1) ("List.length t" 0)))))
        |}]
    ;;

    let%expect_test "duplicate names -> error" =
      init_and_print
        ~names:[ "A"; "A" ]
        ~deck:
          (Deck.Without_exploding_kittens.For_testing.of_card_list
             [ Card.Powerless Beard_cat
             ; Card.Powerless Cattermelon
             ; Card.Powerless Hairy_potato_cat
             ; Card.Powerless Rainbow_ralphing_cat
             ; Card.Powerless Tacocat
             ])
        ~cards_per_player:2;
      [%expect {| (error ("Map.of_alist_or_error: duplicate key" A)) |}]
    ;;

    let%expect_test "duplicate names only matching by case -> error" =
      init_and_print
        ~names:[ "A"; "a" ]
        ~deck:
          (Deck.Without_exploding_kittens.For_testing.of_card_list
             [ Card.Powerless Beard_cat
             ; Card.Powerless Cattermelon
             ; Card.Powerless Hairy_potato_cat
             ; Card.Powerless Rainbow_ralphing_cat
             ; Card.Powerless Tacocat
             ])
        ~cards_per_player:2;
      [%expect {| (error ("Map.of_alist_or_error: duplicate key" a)) |}]
    ;;

    let%expect_test "0 cards per player -> each player has 1 defuse only" =
      init_and_print
        ~names:[ "A"; "B" ]
        ~deck:(Deck.Without_exploding_kittens.For_testing.of_card_list [])
        ~cards_per_player:0;
      [%expect
        {|
        ((deck (Exploding_kitten))
         (player_hands ((A (Playing ((Defuse 1)))) (B (Playing ((Defuse 1)))))))
        |}]
    ;;
  end)
;;

let player_hands_of_alist ~name_and_cards ~eliminated_names =
  let eliminated_list =
    List.map eliminated_names ~f:(fun name -> Player_name.of_string name, Eliminated)
  in
  let playing_list =
    List.map name_and_cards ~f:(fun (name, cards) ->
      Player_name.of_string name, Hand.of_cards cards |> Playing)
  in
  List.append eliminated_list playing_list |> Player_hands.For_testing.of_alist_exn
;;

let add_card_and_print player_hands ~name ~card =
  let player_hands =
    Player_hands.add_card player_hands ~player_name:(Player_name.of_string name) ~card
  in
  print_s [%message (player_hands : Player_hands.t Or_error.t)]
;;

let%expect_test "add tacocat to A -> ok" =
  add_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"A"
    ~card:(Powerless Tacocat);
  [%expect
    {|
    (player_hands
     (Ok
      ((A (Playing ((Defuse 1) ((Powerless Tacocat) 1))))
       (B (Playing (((Power Skip) 1)))))))
    |}]
;;

let%expect_test "add tacocat to A only matching by case -> ok" =
  add_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"a"
    ~card:(Powerless Tacocat);
  [%expect
    {|
    (player_hands
     (Ok
      ((a (Playing ((Defuse 1) ((Powerless Tacocat) 1))))
       (B (Playing (((Power Skip) 1)))))))
    |}]
;;

let%expect_test "add tacocat to A only matching by case -> ok" =
  add_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"a"
    ~card:(Powerless Tacocat);
  [%expect
    {|
    (player_hands
     (Ok
      ((a (Playing ((Defuse 1) ((Powerless Tacocat) 1))))
       (B (Playing (((Power Skip) 1)))))))
    |}]
;;

let%expect_test "add tacocat to unknown player -> error" =
  add_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"unknown_player"
    ~card:(Powerless Tacocat);
  [%expect
    {|
    (player_hands
     (Error
      (("Could not find player name" (player_name unknown_player)
        (t ((A (Playing ((Defuse 1)))) (B (Playing (((Power Skip) 1)))))))
       ("key not found" unknown_player))))
    |}]
;;

let%expect_test "add tacocat to eliminated player -> error" =
  add_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[ "eliminated_player" ])
    ~name:"eliminated_player"
    ~card:(Powerless Tacocat);
  [%expect
    {|
    (player_hands
     (Error ("Player is eliminated" (player_name eliminated_player))))
    |}]
;;

let remove_card_and_print player_hands ~name ~card ~n =
  let player_hands =
    Player_hands.remove_card
      player_hands
      ~player_name:(Player_name.of_string name)
      ~card
      ~n
  in
  print_s [%message (player_hands : Player_hands.t Or_error.t)]
;;

let%expect_test "remove defuse from A -> ok" =
  remove_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"A"
    ~card:Defuse
    ~n:1;
  [%expect {| (player_hands (Ok ((A (Playing ())) (B (Playing (((Power Skip) 1))))))) |}]
;;

let%expect_test "remove multiple defuses from A -> ok" =
  remove_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse; Defuse; Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"A"
    ~card:Defuse
    ~n:3;
  [%expect {| (player_hands (Ok ((A (Playing ())) (B (Playing (((Power Skip) 1))))))) |}]
;;

let%expect_test "remove unowned card from A -> error" =
  remove_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"A"
    ~card:(Power See_the_future)
    ~n:1;
  [%expect
    {|
    (player_hands
     (Error ("Card is not owned" (t ((Defuse 1))) (card (Power See_the_future)))))
    |}]
;;

let%expect_test "remove too many defuses from A -> error" =
  remove_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse; Defuse; Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"A"
    ~card:Defuse
    ~n:4;
  [%expect
    {|
    (player_hands
     (Error ("Not enough copies owned" (t ((Defuse 3))) (card Defuse) (n 4))))
    |}]
;;

let%expect_test "remove card from unknown player -> error" =
  remove_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"unknown_player"
    ~card:(Powerless Tacocat)
    ~n:1;
  [%expect
    {|
    (player_hands
     (Error
      (("Could not find player name" (player_name unknown_player)
        (t ((A (Playing ((Defuse 1)))) (B (Playing (((Power Skip) 1)))))))
       ("key not found" unknown_player))))
    |}]
;;

let%expect_test "remove card from eliminated player -> error" =
  remove_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[ "eliminated_player" ])
    ~name:"eliminated_player"
    ~card:(Powerless Tacocat)
    ~n:1;
  [%expect
    {|
    (player_hands
     (Error ("Player is eliminated" (player_name eliminated_player))))
    |}]
;;

let set_hand_and_print player_hands ~name ~hand =
  let player_hands =
    Player_hands.set_hand player_hands ~player_name:(Player_name.of_string name) ~hand
  in
  print_s [%message (player_hands : Player_hands.t Or_error.t)]
;;

let%expect_test "set hand of A -> ok" =
  set_hand_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"A"
    ~hand:(Hand.of_cards [ Exploding_kitten ]);
  [%expect
    {|
    (player_hands
     (Ok ((A (Playing ((Exploding_kitten 1)))) (B (Playing (((Power Skip) 1)))))))
    |}]
;;

let%expect_test "set hand of unknown player -> error" =
  set_hand_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"unknown_player"
    ~hand:(Hand.of_cards [ Exploding_kitten ]);
  [%expect
    {|
    (player_hands
     (Error
      (("Could not find player name" (player_name unknown_player)
        (t ((A (Playing ((Defuse 1)))) (B (Playing (((Power Skip) 1)))))))
       ("key not found" unknown_player))))
    |}]
;;

let%expect_test "set hand of eliminated player -> error" =
  set_hand_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[ "eliminated_player" ])
    ~name:"eliminated_player"
    ~hand:(Hand.of_cards [ Exploding_kitten ]);
  [%expect
    {|
    (player_hands
     (Error ("Player is eliminated" (player_name eliminated_player))))
    |}]
;;

let transfer_card_and_print player_hands ~receiver ~target =
  match
    Player_hands.transfer_random_card
      player_hands
      ~receiver:(Player_name.of_string receiver)
      ~target:(Player_name.of_string target)
      ~deterministically:true
  with
  | Error error -> print_s [%message (error : Error.t)]
  | Ok (card, player_hands) ->
    print_s [%message (card : Card.t) (player_hands : Player_hands.t)]
;;

let%expect_test "transfer card from B to A -> ok" =
  transfer_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~receiver:"A"
    ~target:"B";
  [%expect
    {|
    ((card (Power Skip))
     (player_hands
      ((A (Playing ((Defuse 1) ((Power Skip) 1)))) (B (Playing ())))))
    |}]
;;

let%expect_test "receiver unknown -> error" =
  transfer_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~receiver:"unknown_player"
    ~target:"B";
  [%expect
    {|
    (error
     (("Could not find player name" (player_name unknown_player)
       (t ((A (Playing ((Defuse 1)))) (B (Playing ())))))
      ("key not found" unknown_player)))
    |}]
;;

let%expect_test "target unknown -> error" =
  transfer_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~receiver:"A"
    ~target:"unknown_player";
  [%expect
    {|
    (error
     (("Could not find player name" (player_name unknown_player)
       (t ((A (Playing ((Defuse 1)))) (B (Playing (((Power Skip) 1)))))))
      ("key not found" unknown_player)))
    |}]
;;

let%expect_test "target has no cards -> error" =
  transfer_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [] ]
       ~eliminated_names:[])
    ~receiver:"A"
    ~target:"B";
  [%expect {| (error ("Target has an empty hand" (target B))) |}]
;;

let%expect_test "receiver is eliminated -> error" =
  transfer_card_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "B", [ Power Skip ] ]
       ~eliminated_names:[ "A" ])
    ~receiver:"A"
    ~target:"B";
  [%expect {| (error ("Player is eliminated" (player_name A))) |}]
;;

let%expect_test "target is eliminated -> error" =
  transfer_card_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ] ] ~eliminated_names:[ "B" ])
    ~receiver:"A"
    ~target:"B";
  [%expect {| (error ("Player is eliminated" (player_name B))) |}]
;;

let eliminate_and_print player_hands ~name =
  let player_hands =
    Player_hands.eliminate player_hands ~player_name:(Player_name.of_string name)
  in
  print_s [%message (player_hands : Player_hands.t Or_error.t)]
;;

let%expect_test "eliminate playing A -> ok" =
  eliminate_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"A";
  [%expect {| (player_hands (Ok ((A Eliminated) (B (Playing (((Power Skip) 1))))))) |}]
;;

let%expect_test "eliminate eliminated A -> ok, nothing happens" =
  eliminate_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "B", [ Power Skip ] ]
       ~eliminated_names:[ "A" ])
    ~name:"A";
  [%expect {| (player_hands (Ok ((A Eliminated) (B (Playing (((Power Skip) 1))))))) |}]
;;

let%expect_test "eliminate unknown player -> error" =
  eliminate_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ]
       ~eliminated_names:[])
    ~name:"unknown_player";
  [%expect
    {|
    (player_hands
     (Error
      (("Could not find player name" (player_name unknown_player)
        (t ((A (Playing ((Defuse 1)))) (B (Playing (((Power Skip) 1)))))))
       ("key not found" unknown_player))))
    |}]
;;

let to_playing_alist_and_print player_hands =
  let alist = Player_hands.to_playing_alist player_hands in
  print_s [%message (alist : (Player_name.t * Hand.t) list)]
;;

let%expect_test "alist ignores eliminated players and looks ok" =
  to_playing_alist_and_print
    (player_hands_of_alist
       ~name_and_cards:[ "A", []; "B", [ Power Skip; Defuse ] ]
       ~eliminated_names:[ "ignored_eliminated_player" ]);
  [%expect {| (alist ((A ()) (B ((Defuse 1) ((Power Skip) 1))))) |}]
;;
