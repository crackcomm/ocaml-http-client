(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Core
open Async
open Http_api
open Common_api

module Exchange_info = Route.Make (struct
  let method_name = "bnc_perp.Public_api.Exchange_info"

  module Request = struct
    type t = unit

    let path = "/fapi/v1/exchangeInfo"
    let method_ = `GET
    let weight = Route.Weight.Static 1
    let query _ = None
  end

  module Response = Route.Make_response_on_success (struct
    type t = Api_t.exchange_info

    let of_string = Api_j.exchange_info_of_string

    module Error_message = Error_message
  end)
end)

let%expect_test "Public Binance Perp API" =
  let pool =
    Http_client.Pool.of_host
      ~size:3
      ~burst_per_second:10
      ~sustained_rate_per_sec:5.0
      "fapi.binance.com"
  in
  Exchange_info.fetch_exn pool ()
  >>| fun info ->
  List.length info.symbols > 150 |> Bool.to_string |> print_endline;
  [%expect {| true |}];
  let btcusdt =
    let symbol = "BTCUSDT" in
    List.find_exn ~f:(fun info -> String.equal info.symbol symbol) info.symbols
  in
  print_s [%sexp (btcusdt : Api_t.symbol_info)];
  [%expect {|
    ((symbol BTCUSDT) (pair BTCUSDT)) |}]
;;
