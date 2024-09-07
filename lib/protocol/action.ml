open! Core
open! Async

module Draw_or_play = struct
  type t =
    | Draw
    | Play_targetless of Card.Power.Targetless.t
    | Play_targeted of (Card.Power.Targeted.t * Player_name.t)
    | Double of (Card.t * Player_name.t)
    | Triple of (Card.t * Player_name.t * Card.t)
  [@@deriving bin_io, sexp, variants]

  (* TODO-someday: Docs of the regex and the regex pattern should be put
     in 1 location so we remember to update the docs and regex together. *)
  let format_doc =
    let add_doc acc _v ~doc = doc :: acc in
    Variants.fold
      ~init:[]
      ~draw:(add_doc ~doc:"draw")
      ~play_targetless:(add_doc ~doc:"CARD")
      ~play_targeted:(add_doc ~doc:"CARD@TARGET_NAME")
      ~double:(add_doc ~doc:"double CARD@TARGET_NAME")
      ~triple:(add_doc ~doc:"triple CARD@TARGET_NAME@TARGET_CARD")
    |> List.rev
    |> String.concat ~sep:"|"
  ;;

  let of_string string =
    let parse_and_accumulate acc ({ name; _ } : _ Variant.t) ~regex ~args_to_t =
      (let%bind.Or_error args =
         Regex.capture_groups_exn ~case_sensitive:false ~regex ~string
         |> Or_error.of_option
              ~error:
                (Error.create_s
                   [%message "Did not match regex pattern" (regex : string)])
       in
       args_to_t args)
      |> Or_error.tag_s
           ~tag:[%message "Could not parse as variant" (name : string)]
      |> fun hd -> hd :: acc
    in
    let unexpected_arg_cnt ~expected_arg_cnt ~args =
      let arg_cnt = List.length args in
      Or_error.error_s
        [%message
          "Unexpected number of args" (expected_arg_cnt : int) (arg_cnt : int)]
    in
    Variants.fold
      ~init:[]
      ~draw:
        (parse_and_accumulate ~regex:"draw" ~args_to_t:(function
          | [] -> Or_error.return Draw
          | args -> unexpected_arg_cnt ~expected_arg_cnt:0 ~args))
      ~play_targetless:
        (parse_and_accumulate ~regex:"(.*)" ~args_to_t:(function
          | [ targetless_card ] ->
            let%map.Or_error targetless_card =
              Card.Power.Targetless.of_string_or_error targetless_card
            in
            Play_targetless targetless_card
          | args -> unexpected_arg_cnt ~expected_arg_cnt:1 ~args))
      ~play_targeted:
        (parse_and_accumulate ~regex:"(.*)@(.*)" ~args_to_t:(function
          | [ targeted_card; target ] ->
            let%bind.Or_error targeted_card =
              Card.Power.Targeted.of_string_or_error targeted_card
            in
            let%map.Or_error target = Player_name.of_string_or_error target in
            Play_targeted (targeted_card, target)
          | args -> unexpected_arg_cnt ~expected_arg_cnt:2 ~args))
      ~double:
        (parse_and_accumulate ~regex:"double (.*)@(.*)" ~args_to_t:(function
          | [ card; target ] ->
            let%bind.Or_error card = Card.of_string_or_error card in
            let%map.Or_error target = Player_name.of_string_or_error target in
            Double (card, target)
          | args -> unexpected_arg_cnt ~expected_arg_cnt:2 ~args))
      ~triple:
        (parse_and_accumulate
           ~regex:"triple (.*)@(.*)@(.*)"
           ~args_to_t:(function
          | [ card; target; target_card ] ->
            let%bind.Or_error card = Card.of_string_or_error card in
            let%bind.Or_error target = Player_name.of_string_or_error target in
            let%map.Or_error target_card =
              Card.of_string_or_error target_card
            in
            Triple (card, target, target_card)
          | args -> unexpected_arg_cnt ~expected_arg_cnt:3 ~args))
    |> Or_error.find_ok
    |> Or_error.tag_s ~tag:[%message "Could not parse string" (string : string)]
  ;;

  (* TODO-soon: This function is getting long. Split the parts out into their
     own function. *)
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
            (Outcome.Exploded, player_hands_with_card_added, deck)
            |> Or_error.return
          | true ->
            let%map.Or_error player_hands =
              Player_hands.remove_card
                player_hands
                ~player_name
                ~card:Defuse
                ~n:1
            in
            Outcome.Defused, player_hands, deck)
       | _ ->
         (Outcome.Drew_safely card, player_hands_with_card_added, deck)
         |> Or_error.return)
    | Play_targetless targetless_card ->
      let%map.Or_error player_hands =
        Player_hands.remove_card
          player_hands
          ~player_name
          ~card:(Power (Targetless targetless_card))
          ~n:1
      in
      (match targetless_card with
       | Attack -> Outcome.Attacked, player_hands, deck
       | See_the_future ->
         Outcome.Saw_the_future (Deck.peek deck ~n:3), player_hands, deck
       | Skip -> Outcome.Skipped, player_hands, deck
       | Shuffle ->
         Outcome.Shuffled, player_hands, Deck.shuffle deck ~deterministically)
    | Play_targeted (targeted_card, target) ->
      if Player_name.equal player_name target
      then
        Or_error.error_s
          [%message
            "Target cannot be the performing player"
              (player_name : Player_name.t)
              (target : Player_name.t)]
      else if Player_hands.is_playing player_hands ~player_name:target |> not
      then
        Or_error.error_s
          [%message "Target must be playing" (target : Player_name.t)]
      else (
        let%map.Or_error player_hands =
          Player_hands.remove_card
            player_hands
            ~player_name
            ~card:(Power (Targeted targeted_card))
            ~n:1
        in
        match targeted_card with
        | Favor -> Outcome.Favored target, player_hands, deck)
    | Double (card, target) ->
      let%bind.Or_error player_hands =
        Player_hands.remove_card player_hands ~player_name ~card ~n:2
      in
      let%map.Or_error stolen_card, player_hands =
        Player_hands.transfer_random_card
          player_hands
          ~receiver:player_name
          ~target
          ~deterministically:false
      in
      ( Outcome.Stole_randomly_via_double (card, target, stolen_card)
      , player_hands
      , deck )
    | Triple (card, target, target_card) ->
      let%bind.Or_error player_hands =
        Player_hands.remove_card player_hands ~player_name ~card ~n:3
      in
      (match%bind.Or_error
         Player_hands.has_card
           player_hands
           ~player_name:target
           ~card:target_card
       with
       | false ->
         ( Outcome.Failed_to_steal_via_triple (card, target, target_card)
         , player_hands
         , deck )
         |> Or_error.return
       | true ->
         let%map.Or_error player_hands =
           Player_hands.transfer_card
             player_hands
             ~receiver:player_name
             ~target
             ~card:target_card
         in
         ( Outcome.Stole_via_triple (card, target, target_card)
         , player_hands
         , deck ))
  ;;

  module For_testing = struct
    let all_mocked ~play_targeted_target ~double ~triple =
      Variants.fold
        ~init:[]
        ~draw:Variants_helper.accumulate_without_args
        ~play_targetless:(fun acc v ->
          List.map Card.Power.Targetless.all ~f:v.constructor |> List.append acc)
        ~play_targeted:(fun acc v ->
          List.map Card.Power.Targeted.all ~f:(fun targeted_card ->
            v.constructor (targeted_card, play_targeted_target))
          |> List.append acc)
        ~double:(Variants_helper.accumulate_with_args ~args:double)
        ~triple:(Variants_helper.accumulate_with_args ~args:triple)
    ;;
  end
end

module Insert_exploding_kitten = struct
  let handle ~position ~deck =
    ( Outcome.Inserted_exploding_kitten position
    , Deck.insert deck ~card:Card.Exploding_kitten ~position )
  ;;
end

module Give_a_card = struct
  let handle ~player_hands ~receiver ~target ~card =
    let%map.Or_error player_hands =
      Player_hands.transfer_card player_hands ~receiver ~target ~card
    in
    Outcome.Received_card_from (card, target), player_hands
  ;;
end
