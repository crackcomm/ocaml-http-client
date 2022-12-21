(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
include Signed_route_intf

module Make (E : Descriptor) :
  S
    with type Request.t := E.Request.t
     and type Response.t := E.Response.t
     and type Signer.t := E.Signer.t = struct
  module Signer = E.Signer
  module R = Route.Make (E)
  include R

  let http_request signer input = Signer.sign_request signer (http_request input)

  let fetch_response ?timeout client signer req =
    Http_client.call
      ~method_name:E.method_name
      ?timeout
      ~weight:(Request.weight req)
      client
      (http_request signer req)
  ;;

  let call ?timeout client signer req =
    fetch_response ?timeout client signer req
    >>= function
    | Ok resp -> Response.parse_response resp.status resp.body >>| Result.return
    | Error err -> return (Error err)
  ;;

  let dispatch_exn ?timeout client signer req =
    fetch_response ?timeout client signer req
    >>| Http_types.Response.Result.ok_exn ~here:[%here]
    >>= fun resp -> Response.parse_response resp.status resp.body
  ;;
end
