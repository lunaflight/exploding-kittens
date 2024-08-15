open! Core
open! Async
open Protocol_lib

let command =
  Command.async
    ~summary:"Start a player process"
    (let%map_open.Command port =
       flag "port" (required int) ~doc:"PORT the port to listen for RPCs"
     and name = flag "name" (required string) ~doc:"STRING your name" in
     fun () ->
       let implementations =
         Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Close_connection
           ~implementations:
             [ Rpc.Rpc.implement Rpcs.Get_action.rpc (fun _client hand ->
                 Logic.get_action ~hand)
             ; Rpc.Rpc.implement Rpcs.Message.rpc (fun _client message ->
                 Logic.print_string message |> return)
             ; Rpc.Rpc.implement Rpcs.Name.rpc (fun _client () -> return name)
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
