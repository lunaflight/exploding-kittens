open! Core
open! Async

module Message = struct
  module Query = struct
    type t = string [@@deriving sexp, bin_io]

    let%expect_test "bin_digest" =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}];
      return ()
    ;;
  end

  module Response = struct
    type t = unit [@@deriving sexp, bin_io]

    let%expect_test "bin_digest" =
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
