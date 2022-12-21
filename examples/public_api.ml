(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
open Http_api

module Error_message = Response.Make (struct
  type t = Http_api_message.t [@@deriving sexp_of]

  let of_string s =
    let res = Api_j.response_message_of_string s in
    { Http_api_message.code = res.code; message = res.message }
  ;;
end)

module Exchange_info = Route.Make (struct
  let method_name = "bnc_perp.Public_api.Exchange_info"

  module Request = Request.Make_get (struct
    type t = unit [@@deriving sexp_of]

    let path _ = "/fapi/v1/exchangeInfo"
    let weight _ = 1
    let query _ = None
  end)

  module Response = Response.Make_on_success (struct
    include Response.Make (struct
      type t = Api_t.exchange_info [@@deriving sexp_of]

      let of_string = Api_j.exchange_info_of_string
    end)

    module Response_error = Error_message
  end)
end)

let%expect_test "Public Binance Perp API" =
  let pool =
    Http_client.create_test
      (module Cohttp_backend.Client)
      (Uri.make ~scheme:"https" ~host:"fapi.binance.com" ())
  in
  Exchange_info.dispatch_exn pool ()
  >>| Http_api_message.Result.value_exn ~here:[%here]
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
