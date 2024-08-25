open! Core

type t =
  | Defused
  | Drew_safely of Card.t
  | Exploded
  | Inserted_exploding_kitten of int
  | Saw_the_future of Card.t list
  | Skipped
  | Shuffled
[@@deriving variants, sexp_of]

let to_self_alert = function
  | Defused -> [%string "You defused an exploding kitten!"]
  (* TODO-someday: Perhaps we can provide a lookup table to decide between "a" or
     "an" or download some library that handles this for us. *)
  | Drew_safely card -> [%string "You drew a(n) %{card#Card}."]
  | Inserted_exploding_kitten position ->
    [%string "You inserted an exploding kitten at position %{position#Int}."]
  | Exploded -> [%string "You exploded!"]
  | Saw_the_future cards ->
    let cards_string = List.map cards ~f:Card.to_string |> String.concat ~sep:", " in
    let n_cards =
      match List.length cards with
      | 1 -> "1 card"
      | cnt -> [%string "%{cnt#Int} cards"]
    in
    (match List.length cards with
     | 0 -> "You did not see any cards as the deck is empty."
     | _ -> [%string "You saw %{n_cards} at the top of the deck: %{cards_string}"])
  | Skipped -> [%string "You skipped your turn."]
  | Shuffled -> [%string "You shuffled the deck."]
;;

let to_others_alert t ~player_name =
  match t with
  | Defused -> [%string "%{player_name#Player_name} defused an exploding kitten!"]
  | Drew_safely _ -> [%string "%{player_name#Player_name} drew a card."]
  | Inserted_exploding_kitten _ ->
    [%string "%{player_name#Player_name} inserted an exploding kitten somewhere."]
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
;;

module For_testing = struct
  let all_mocked ~drew_safely ~inserted_exploding_kitten ~saw_the_future =
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
  ;;
end
