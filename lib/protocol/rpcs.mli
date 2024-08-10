open! Core
open! Async

module Message : sig
  module Query : sig
    type t = string
  end

  module Response : sig
    type t = unit
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end
