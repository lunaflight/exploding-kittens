open! Core
open Protocol_lib

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
        ((Defuse 1) ((Powerless Rainbow_ralphing_cat) 1) ((Powerless Tacocat) 1)))
       (B
        ((Defuse 1) ((Powerless Cattermelon) 1) ((Powerless Hairy_potato_cat) 1))))))
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
        ((Defuse 1) ((Powerless Rainbow_ralphing_cat) 1) ((Powerless Tacocat) 1))))))
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

    let%expect_test "0 cards per player -> each player has 1 defuse only" =
      init_and_print
        ~names:[ "A"; "B" ]
        ~deck:(Deck.Without_exploding_kittens.For_testing.of_card_list [])
        ~cards_per_player:0;
      [%expect
        {|
    ((deck (Exploding_kitten))
     (player_hands ((A ((Defuse 1))) (B ((Defuse 1))))))
    |}]
    ;;
  end)
;;

let player_hands_of_alist ~name_and_cards =
  List.map name_and_cards ~f:(fun (name, cards) ->
    Player_name.of_string name, Hand.of_cards cards)
  |> Player_hands.For_testing.of_alist_exn
;;

let add_card_and_print player_hands ~name ~card =
  let player_hands =
    Player_hands.add_card player_hands ~player_name:(Player_name.of_string name) ~card
  in
  print_s [%message (player_hands : Player_hands.t Or_error.t)]
;;

let%expect_test "add tacocat to A -> ok" =
  add_card_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ])
    ~name:"A"
    ~card:(Powerless Tacocat);
  [%expect
    {|
    (player_hands
     (Ok ((A ((Defuse 1) ((Powerless Tacocat) 1))) (B (((Power Skip) 1))))))
    |}]
;;

let%expect_test "add tacocat to unknown player -> error" =
  add_card_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ])
    ~name:"unknown_player"
    ~card:(Powerless Tacocat);
  [%expect {| (player_hands (Error ("key not found" unknown_player))) |}]
;;

let remove_card_and_print player_hands ~name ~card =
  let player_hands =
    Player_hands.remove_card player_hands ~player_name:(Player_name.of_string name) ~card
  in
  print_s [%message (player_hands : Player_hands.t Or_error.t)]
;;

let%expect_test "remove defuse from A -> ok" =
  remove_card_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ])
    ~name:"A"
    ~card:Defuse;
  [%expect {| (player_hands (Ok ((A ()) (B (((Power Skip) 1)))))) |}]
;;

let%expect_test "remove unowned card from A -> error" =
  remove_card_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ])
    ~name:"A"
    ~card:(Power See_the_future);
  [%expect
    {|
    (player_hands
     (Error ("Card is not owned" (card (Power See_the_future)) (t ((Defuse 1))))))
    |}]
;;

let%expect_test "remove card from unknown player -> error" =
  remove_card_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ])
    ~name:"unknown_player"
    ~card:(Powerless Tacocat);
  [%expect {| (player_hands (Error ("key not found" unknown_player))) |}]
;;

let set_hand_and_print player_hands ~name ~hand =
  let player_hands =
    Player_hands.set_hand player_hands ~player_name:(Player_name.of_string name) ~hand
  in
  print_s [%message (player_hands : Player_hands.t Or_error.t)]
;;

let%expect_test "set hand of A -> ok" =
  set_hand_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ])
    ~name:"A"
    ~hand:(Hand.of_cards [ Exploding_kitten ]);
  [%expect {| (player_hands (Ok ((A ((Exploding_kitten 1))) (B (((Power Skip) 1)))))) |}]
;;

let%expect_test "set hand of unknown player -> error" =
  set_hand_and_print
    (player_hands_of_alist ~name_and_cards:[ "A", [ Defuse ]; "B", [ Power Skip ] ])
    ~name:"unknown_player"
    ~hand:(Hand.of_cards [ Exploding_kitten ]);
  [%expect {| (player_hands (Error ("key not found" unknown_player))) |}]
;;
