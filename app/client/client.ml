open! Core
open! Async
open Protocol_lib

let command =
  Command.async
    ~summary:"Start a player process"
    (let%map_open.Command port =
       flag "port" (required int) ~doc:"PORT the port to listen for RPCs"
     in
     fun () ->
       let get_action_implementation _client cards =
         print_s [%message "Make an action" (cards : Card.t list)];
         (* TODO: Ask the player for an action *)
         return Action.Draw
       in
       let message_implementation _client message =
         print_s [%message "Received message" (message : string)];
         Deferred.return ()
       in
       let implementations =
         Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Close_connection
           ~implementations:
             [ Rpc.Rpc.implement Rpcs.Get_action.rpc get_action_implementation
             ; Rpc.Rpc.implement Rpcs.Message.rpc message_implementation
             ]
       in
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun addr _conn -> addr)
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;
