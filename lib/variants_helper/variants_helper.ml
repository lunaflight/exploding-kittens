open! Core

let accumulate_without_args acc (v : 'a Variant.t) = acc @ [ v.constructor ]

let accumulate_with_args acc (v : ('b -> 'a) Variant.t) ~args =
  List.map args ~f:v.constructor |> List.append acc
;;
