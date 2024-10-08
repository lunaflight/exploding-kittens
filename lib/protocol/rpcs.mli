open! Core
open! Async

(** Rpc for the server to ask the player name of a client *)
module Player_name : sig
  module Query : sig
    type t = unit
  end

  module Response : sig
    type t = Player_name.t
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

(** Rpc for the server to send a message to a client. *)
module Message : sig
  module Query : sig
    type t = string
  end

  module Response : sig
    type t = unit
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

(** Rpc for the server to ask a client for a card to give given their hand. *)
module Get_card_to_give : sig
  module Query : sig
    type t = Hand.t
  end

  module Response : sig
    type t = Card.t
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

(** Rpc for the server to ask a client for an action given their hand. *)
module Get_draw_or_play : sig
  module Query : sig
    type t = Hand.t
  end

  module Response : sig
    type t = Action.Draw_or_play.t
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

(** Rpc for the server to ask a client for an exploding kitten insert position
    given the deck size. *)
module Get_exploding_kitten_insert_position : sig
  module Query : sig
    type t = int
  end

  module Response : sig
    type t = int
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end
