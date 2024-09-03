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
             [ Rpc.Rpc.implement Rpcs.Get_card_to_give.rpc (fun _client hand ->
                 Logic.get_card_to_give ~hand)
             ; Rpc.Rpc.implement Rpcs.Get_draw_or_play.rpc (fun _client hand ->
                 Logic.get_draw_or_play ~hand)
             ; Rpc.Rpc.implement
                 Rpcs.Get_exploding_kitten_insert_position.rpc
                 (fun _client deck_size ->
                    Logic.get_exploding_kitten_insert_position ~deck_size)
             ; Rpc.Rpc.implement Rpcs.Message.rpc (fun _client message ->
                 Logic.print_string message |> return)
             ; Rpc.Rpc.implement Rpcs.Player_name.rpc (fun _client () ->
                 Player_name.of_string_exn name |> return)
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
