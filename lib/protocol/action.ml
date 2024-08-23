open! Core
open! Async

module Draw_or_play = struct
  type t =
    | Draw
    | Play of Card.Power.t
    | Double of (Card.t * Player_name.t)
  [@@deriving bin_io, sexp, variants]

  let format_doc =
    let add_doc acc _v ~doc = doc :: acc in
    Variants.fold
      ~init:[]
      ~draw:(add_doc ~doc:"draw")
      ~play:(add_doc ~doc:"CARD")
      ~double:(add_doc ~doc:"double CARD@TARGET_NAME")
    |> List.rev
    |> String.concat ~sep:"|"
  ;;

  let to_string = function
    | Draw -> "Draw"
    | Play power -> Card.Power.to_string power
    | Double (card, target) -> [%string "Double %{card#Card}@%{target#Player_name}"]
  ;;

  let of_string string =
    (* TODO-soon: Use the re2 library
       [https://ocaml.org/p/re2/latest/doc/Re2/index.html]
       or some [Scanf] magic to make this more readable
       and maintainable. *)
    if String.Caseless.equal string "draw"
    then Or_error.return Draw
    else (
      match String.Caseless.is_prefix string ~prefix:"double " with
      | true ->
        let%bind.Or_error card, target =
          String.Caseless.substr_replace_first string ~pattern:"double " ~with_:""
          |> String.lsplit2 ~on:'@'
          |> Or_error.of_option ~error:(Error.of_string "Expected format: card@target")
        in
        let%map.Or_error card = Card.of_string_or_error card in
        Double (card, Player_name.of_string target)
      | false ->
        let%map.Or_error power = Card.Power.of_string_or_error string in
        Play power)
  ;;

  let handle t ~player_hands ~player_name ~deck ~deterministically =
    match t with
    | Draw ->
      let%bind.Or_error card, deck = Deck.draw deck in
      let%bind.Or_error player_hands_with_card_added =
        Player_hands.add_card player_hands ~player_name ~card
      in
      (match card with
       | Exploding_kitten ->
         (match%bind.Or_error
            Player_hands.has_card player_hands ~player_name ~card:Defuse
          with
          | false ->
            (Outcome.Exploded, player_hands_with_card_added, deck) |> Or_error.return
          | true ->
            let%map.Or_error player_hands =
              Player_hands.remove_card player_hands ~player_name ~card:Defuse ~n:1
            in
            Outcome.Defused, player_hands, deck)
       | _ ->
         (Outcome.Drew_safely card, player_hands_with_card_added, deck) |> Or_error.return)
    | Play power ->
      let%map.Or_error player_hands =
        Player_hands.remove_card player_hands ~player_name ~card:(Power power) ~n:1
      in
      (match power with
       | See_the_future ->
         Outcome.Saw_the_future (Deck.peek deck ~n:3), player_hands, deck
       | Skip -> Outcome.Skipped, player_hands, deck
       | Shuffle -> Outcome.Shuffled, player_hands, Deck.shuffle deck ~deterministically)
    | Double (card, target) ->
      let%bind.Or_error player_hands =
        Player_hands.remove_card player_hands ~player_name ~card ~n:2
      in
      let%map.Or_error card, player_hands =
        Player_hands.transfer_random_card
          player_hands
          ~receiver:player_name
          ~target
          ~deterministically:false
      in
      Outcome.Stole_randomly (card, target), player_hands, deck
  ;;

  module For_testing = struct
    let all_mocked ~double_target =
      Variants.fold
        ~init:[]
        ~draw:(fun acc v -> acc @ [ v.constructor ])
        ~play:(fun acc v -> List.map Card.Power.all ~f:v.constructor |> List.append acc)
        ~double:(fun acc v ->
          Card.all
          |> List.map ~f:(fun power -> power, double_target)
          |> List.map ~f:v.constructor
          |> List.append acc)
    ;;
  end
end

module Insert_exploding_kitten = struct
  let handle ~position ~deck =
    ( Outcome.Inserted_exploding_kitten position
    , Deck.insert deck ~card:Card.Exploding_kitten ~position )
  ;;
end
