open! Core

type t =
  | Defused
  | Drew_safely of Card.t
  | Exploded
  | Failed_to_steal_via_triple of (Card.t * Player_name.t * Card.t)
  | Inserted_exploding_kitten of int
  | Saw_the_future of Card.t list
  | Shuffled
  | Skipped
  | Stole_randomly_via_double of (Card.t * Player_name.t * Card.t)
  | Stole_via_triple of (Card.t * Player_name.t * Card.t)
[@@deriving variants, sexp_of]

let fill_uncensored_alert_template t ~player_name ~possessive_pronoun =
  match t with
  | Defused ->
    [%string "%{player_name#Player_name} defused an exploding kitten!"]
  (* TODO-someday: Perhaps we can provide a lookup table to decide between "a"
     or "an" or download some library that handles this for us. *)
  | Drew_safely card ->
    [%string "%{player_name#Player_name} drew a(n) %{card#Card}."]
  | Inserted_exploding_kitten position ->
    [%string
      "%{player_name#Player_name} inserted an exploding kitten at position \
       %{position#Int}."]
  | Exploded -> [%string "%{player_name#Player_name} exploded!"]
  | Failed_to_steal_via_triple (card, target, target_card) ->
    [%string
      "%{player_name#Player_name} played a(n) %{card#Card} triple and failed \
       to steal a(n) %{target_card#Card} from %{target#Player_name}."]
  | Saw_the_future cards ->
    let cards_string =
      List.map cards ~f:Card.to_string |> String.concat ~sep:", "
    in
    let n_cards =
      match List.length cards with
      | 1 -> "1 card"
      | cnt -> [%string "%{cnt#Int} cards"]
    in
    (match List.length cards with
     | 0 ->
       [%string
         "%{player_name#Player_name} did not see any cards as the deck is \
          empty."]
     | _ ->
       [%string
         "%{player_name#Player_name} saw %{n_cards} at the top of the deck: \
          %{cards_string}."])
  | Shuffled -> [%string "%{player_name#Player_name} shuffled the deck."]
  | Skipped ->
    [%string "%{player_name#Player_name} skipped %{possessive_pronoun} turn."]
  | Stole_randomly_via_double (card, target, stolen_card) ->
    [%string
      "%{player_name#Player_name} played a(n) %{card#Card} double and randomly \
       stole a(n) %{stolen_card#Card} from %{target#Player_name}."]
  | Stole_via_triple (card, target, stolen_card) ->
    [%string
      "%{player_name#Player_name} played a(n) %{card#Card} triple and stole \
       a(n) %{stolen_card#Card} from %{target#Player_name}."]
;;

let to_uncensored_alert t ~player_name =
  fill_uncensored_alert_template t ~player_name ~possessive_pronoun:"their"
;;

let to_self_alert t =
  fill_uncensored_alert_template
    t
    ~player_name:(Player_name.of_string_exn "You")
    ~possessive_pronoun:"your"
;;

let to_specialised_alert t ~player_name =
  match t with
  | Defused
  | Drew_safely _
  | Exploded
  | Inserted_exploding_kitten _
  | Saw_the_future _
  | Shuffled
  | Skipped -> None
  | Failed_to_steal_via_triple (card, target, target_card) ->
    ( target
    , (card, Player_name.of_string_exn "you", target_card)
      |> Failed_to_steal_via_triple
      |> to_uncensored_alert ~player_name )
    |> Some
  | Stole_randomly_via_double (card, target, stolen_card) ->
    ( target
    , (card, Player_name.of_string_exn "you", stolen_card)
      |> Stole_randomly_via_double
      |> to_uncensored_alert ~player_name )
    |> Some
  | Stole_via_triple (card, target, stolen_card) ->
    ( target
    , (card, Player_name.of_string_exn "you", stolen_card)
      |> Stole_via_triple
      |> to_uncensored_alert ~player_name )
    |> Some
;;

let to_censored_alert t ~player_name =
  match t with
  | Defused
  | Exploded
  | Failed_to_steal_via_triple _
  | Shuffled
  | Skipped
  | Stole_via_triple _ -> to_uncensored_alert t ~player_name
  | Drew_safely _card -> [%string "%{player_name#Player_name} drew a card."]
  | Inserted_exploding_kitten _position ->
    [%string
      "%{player_name#Player_name} inserted an exploding kitten somewhere."]
    (* TODO-soon: Upon death, players should be able to see their hand. *)
  | Saw_the_future cards ->
    let n_cards =
      match List.length cards with
      | 1 -> "1 card"
      | cnt -> [%string "%{cnt#Int} cards"]
    in
    [%string
      "%{player_name#Player_name} saw the future of %{n_cards} at the top of \
       the deck."]
  | Stole_randomly_via_double (card, target, _stolen_card) ->
    [%string
      "%{player_name#Player_name} played a(n) %{card#Card} double and randomly \
       stole a card from %{target#Player_name}."]
;;

module For_testing = struct
  let all_mocked
    ~drew_safely
    ~failed_to_steal_via_triple
    ~inserted_exploding_kitten
    ~saw_the_future
    ~stole_randomly_via_double
    ~stole_via_triple
    =
    Variants.fold
      ~init:[]
      ~defused:Variants_helper.accumulate_without_args
      ~drew_safely:(Variants_helper.accumulate_with_args ~args:drew_safely)
      ~exploded:Variants_helper.accumulate_without_args
      ~failed_to_steal_via_triple:
        (Variants_helper.accumulate_with_args ~args:failed_to_steal_via_triple)
      ~inserted_exploding_kitten:
        (Variants_helper.accumulate_with_args ~args:inserted_exploding_kitten)
      ~saw_the_future:
        (Variants_helper.accumulate_with_args ~args:saw_the_future)
      ~skipped:Variants_helper.accumulate_without_args
      ~shuffled:Variants_helper.accumulate_without_args
      ~stole_via_triple:
        (Variants_helper.accumulate_with_args ~args:stole_via_triple)
      ~stole_randomly_via_double:
        (Variants_helper.accumulate_with_args ~args:stole_randomly_via_double)
  ;;
end
