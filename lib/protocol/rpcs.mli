open! Core
open! Async

(** Rpc for the server to ask the name of a client *)
module Name : sig
  module Query : sig
    type t = unit
  end

  module Response : sig
    type t = string
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

(** Rpc for the server to ask a client for an action given their hand. *)
module Get_action : sig
  module Query : sig
    type t = Hand.t
  end

  module Response : sig
    type t = Action.t
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end
