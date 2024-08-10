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
       let message_implementation _client message =
         print_s [%message "Received message" (message : string)];
         Deferred.return ()
       in
       let implementations =
         Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Close_connection
           ~implementations:[ Rpc.Rpc.implement Rpcs.Message.rpc message_implementation ]
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
