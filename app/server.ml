open! Core
open! Async
open Protocol_lib

let command =
  Command.async
    ~summary:"Start a server process"
    (let%map_open.Command player =
       flag
         "player"
         (one_or_more_as_list (Command.Arg_type.create Host_and_port.of_string))
         ~doc:"HOST_AND_PORT (format=host:port) Exactly two must be provided."
     in
     fun () ->
       match List.permute player with
       | [ player_1; player_2 ] ->
         print_s [%message (player_1 : Host_and_port.t) (player_2 : Host_and_port.t)];
         let%bind connection_1 =
           Rpc.Connection.client (Tcp.Where_to_connect.of_host_and_port player_1)
           >>| Result.ok_exn
         in
         let%bind connection_2 =
           Rpc.Connection.client (Tcp.Where_to_connect.of_host_and_port player_2)
           >>| Result.ok_exn
         in
         let%bind () =
           Rpc.Rpc.dispatch_exn Rpcs.Message.rpc connection_1 "hello connection 1"
         in
         let%bind () =
           Rpc.Rpc.dispatch_exn Rpcs.Message.rpc connection_2 "hello connection 2"
         in
         Deferred.never ()
       | _ ->
         raise_s
           [%message
             "incorrect number of players specified" (player : Host_and_port.t list)])
;;
