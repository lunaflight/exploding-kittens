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
       match player with
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
         let%bind connector =
           Connector.of_connections connections |> Deferred.Or_error.ok_exn
         in
         Game_state.start_game
           ~connector
           ~get_draw_or_play:(Interaction.get_draw_or_play_exn connector)
           ~get_exploding_kitten_insert_position:
             (Interaction.get_exploding_kitten_insert_position_exn connector)
           ~on_outcome:(Interaction.broadcast_to_players_exn connector)
           ~on_win:(fun ~player_name ~message ->
             Connector.send_message connector ~player_name ~message
             |> Deferred.Or_error.ok_exn)
         |> Deferred.Or_error.ok_exn)
;;
