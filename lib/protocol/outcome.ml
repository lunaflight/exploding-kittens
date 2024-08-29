open! Core

type t =
  | Defused
  | Drew_safely of Card.t
  | Exploded
  | Inserted_exploding_kitten of int
  | Saw_the_future of Card.t list
  | Skipped
  | Shuffled
  | Stole_randomly of (Card.t * Player_name.t)
[@@deriving variants, sexp_of]

let fill_uncensored_alert_template t ~player_name ~possessive_pronoun =
  match t with
  | Defused -> [%string "%{player_name#Player_name} defused an exploding kitten!"]
  (* TODO-someday: Perhaps we can provide a lookup table to decide between "a" or
     "an" or download some library that handles this for us. *)
  | Drew_safely card -> [%string "%{player_name#Player_name} drew a(n) %{card#Card}."]
  | Inserted_exploding_kitten position ->
    [%string
      "%{player_name#Player_name} inserted an exploding kitten at position \
       %{position#Int}."]
  | Exploded -> [%string "%{player_name#Player_name} exploded!"]
  | Saw_the_future cards ->
    let cards_string = List.map cards ~f:Card.to_string |> String.concat ~sep:", " in
    let n_cards =
      match List.length cards with
      | 1 -> "1 card"
      | cnt -> [%string "%{cnt#Int} cards"]
    in
    (match List.length cards with
     | 0 ->
       [%string "%{player_name#Player_name} did not see any cards as the deck is empty."]
     | _ ->
       [%string
         "%{player_name#Player_name} saw %{n_cards} at the top of the deck: \
          %{cards_string}."])
  | Skipped -> [%string "%{player_name#Player_name} skipped %{possessive_pronoun} turn."]
  | Shuffled -> [%string "%{player_name#Player_name} shuffled the deck."]
  | Stole_randomly (card, target) ->
    [%string
      "%{player_name#Player_name} stole a(n) %{card#Card} from %{target#Player_name}."]
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
  | Defused | Exploded | Skipped | Shuffled -> None
  | Drew_safely _card -> None
  | Inserted_exploding_kitten _position -> None
  | Saw_the_future _cards -> None
  | Stole_randomly (card, target) ->
    (target, [%string "%{player_name#Player_name} stole a(n) %{card#Card} from you."])
    |> Some
;;

let to_censored_alert t ~player_name =
  match t with
  | Defused -> [%string "%{player_name#Player_name} defused an exploding kitten!"]
  | Drew_safely _card -> [%string "%{player_name#Player_name} drew a card."]
  | Inserted_exploding_kitten _position ->
    [%string "%{player_name#Player_name} inserted an exploding kitten somewhere."]
    (* TODO-soon: Upon death, players should be able to see their hand. *)
  | Exploded -> [%string "%{player_name#Player_name} exploded!"]
  | Saw_the_future cards ->
    let n_cards =
      match List.length cards with
      | 1 -> "1 card"
      | cnt -> [%string "%{cnt#Int} cards"]
    in
    [%string
      "%{player_name#Player_name} saw the future of %{n_cards} at the top of the deck."]
  | Skipped -> [%string "%{player_name#Player_name} skipped their turn."]
  | Shuffled -> [%string "%{player_name#Player_name} shuffled the deck."]
  | Stole_randomly (_card, target) ->
    [%string "%{player_name#Player_name} stole a random card from %{target#Player_name}."]
;;

module For_testing = struct
  let all_mocked ~drew_safely ~inserted_exploding_kitten ~saw_the_future ~stole_randomly =
    Variants.fold
      ~init:[]
      ~defused:(fun acc v -> acc @ [ v.constructor ])
      ~drew_safely:(fun acc v -> List.map drew_safely ~f:v.constructor |> List.append acc)
      ~exploded:(fun acc v -> acc @ [ v.constructor ])
      ~inserted_exploding_kitten:(fun acc v ->
        List.map inserted_exploding_kitten ~f:v.constructor |> List.append acc)
      ~saw_the_future:(fun acc v ->
        List.map saw_the_future ~f:v.constructor |> List.append acc)
      ~skipped:(fun acc v -> acc @ [ v.constructor ])
      ~shuffled:(fun acc v -> acc @ [ v.constructor ])
      ~stole_randomly:(fun acc v ->
        List.map stole_randomly ~f:v.constructor |> List.append acc)
  ;;
end
