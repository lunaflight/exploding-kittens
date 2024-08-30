open! Core
open! Async

module Player_name = struct
  module Query = struct
    type t = unit [@@deriving sexp, bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 86ba5df747eec837f0b391dd49f33f9e |}];
      return ()
    ;;
  end

  module Response = struct
    type t = Player_name.t [@@deriving sexp, bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}];
      return ()
    ;;
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"name"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Rpc.How_to_recognise_errors.Only_on_exn
  ;;
end

module Message = struct
  module Query = struct
    type t = string [@@deriving sexp, bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}];
      return ()
    ;;
  end

  module Response = struct
    type t = unit [@@deriving sexp, bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 86ba5df747eec837f0b391dd49f33f9e |}];
      return ()
    ;;
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"send_message"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Rpc.How_to_recognise_errors.Only_on_exn
  ;;
end

module Get_draw_or_play = struct
  module Query = struct
    type t = Hand.t [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 17b4e8fbfde34de07accdb9e9e6693db |}];
      return ()
    ;;
  end

  module Response = struct
    type t = Action.Draw_or_play.t [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 9c89578cf036ac285be09274d32ef03e |}];
      return ()
    ;;
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get_draw_or_play"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Rpc.How_to_recognise_errors.Only_on_exn
  ;;
end

module Get_exploding_kitten_insert_position = struct
  module Query = struct
    type t = int [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 698cfa4093fe5e51523842d37b92aeac |}];
      return ()
    ;;
  end

  module Response = struct
    type t = int [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 698cfa4093fe5e51523842d37b92aeac |}];
      return ()
    ;;
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get_exploding_kitten_insert_position"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Rpc.How_to_recognise_errors.Only_on_exn
  ;;
end
