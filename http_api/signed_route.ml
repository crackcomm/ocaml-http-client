(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
include Signed_route_intf

module Make (C : Signer) (E : Route_intf.Descriptor) :
  S
    with type Request.t := E.Request.t
     and type Response.t = E.Response.t
     and type Response.Error.t = E.Response.Error.t
     and type Signer.t := C.t = struct
  module Signer = C
  module R = Route.Make (E)
  include R
  include E

  module Error = struct
    type t =
      [ `Message of Response.Error.t
      | Http_types.Error.t
      ]
    [@@deriving sexp_of]
  end

  let http_request signer input = Signer.sign_request signer (http_request input)

  let fetch_response ?timeout client signer req =
    Http_client.call
      ~method_name:E.method_name
      ?timeout
      ~weight:(Request.weight req)
      client
      (http_request signer req)
  ;;

  let dispatch ?timeout client signer req =
    fetch_response ?timeout client signer req
    >>= function
    | Ok resp ->
      Response.parse_response resp.status resp.body
      >>| (function
      | Ok res -> Ok res
      | Error err -> Error (`Message err))
    | Error err -> return (Error (err :> Error.t))
  ;;

  let dispatch_exn ?(here = [%here]) ?timeout client signer req =
    dispatch ?timeout client signer req
    >>| function
    | Ok resp -> resp
    | Error err -> failwiths ~here "Signed_route.dispatch_exn" err Error.sexp_of_t
  ;;
end
