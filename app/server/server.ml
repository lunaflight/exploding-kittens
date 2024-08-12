open! Core
open! Async

let command =
  Command.async
    ~summary:"Start a server process"
    (let%map_open.Command player =
       flag
         "player"
         (one_or_more_as_list (Command.Arg_type.create Host_and_port.of_string))
         ~doc:"HOST_AND_PORT (format=host:port) More than one must be provided."
     in
     fun () ->
       match List.permute player with
       | [] | [ _ ] ->
         raise_s
           [%message
             "incorrect number of players specified" (player : Host_and_port.t list)]
       | players ->
         let%bind connections =
           Deferred.List.map ~how:(`Max_concurrent_jobs 16) players ~f:(fun player ->
             Rpc.Connection.client (Tcp.Where_to_connect.of_host_and_port player)
             >>| Result.ok_exn)
         in
         Logic.start ~connections)
;;
