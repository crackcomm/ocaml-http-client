(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
include Route_intf

module Make (E : Descriptor) = struct
  include E

  module Error = struct
    type t =
      [ `Message of Response.Error.t
      | Http_types.Error.t
      ]
    [@@deriving sexp_of]
  end

  let http_request = Request.request

  let fetch_response ?timeout client req =
    Http_client.call
      ~method_name:E.method_name
      ?timeout
      ~weight:(Request.weight req)
      client
      (http_request req)
  ;;

  let dispatch ?timeout client req =
    fetch_response ?timeout client req
    >>= function
    | Ok resp ->
      Response.parse_response resp.status resp.body
      >>| (function
      | Ok res -> Ok res
      | Error err -> Error (`Message err))
    | Error err -> return (Error (err :> Error.t))
  ;;

  let dispatch_exn ?(here = [%here]) ?timeout client req =
    dispatch ?timeout client req
    >>| function
    | Ok resp -> resp
    | Error err -> failwiths ~here "Route.dispatch_exn" err Error.sexp_of_t
  ;;
end
