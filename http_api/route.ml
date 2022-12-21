(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
include Route_intf

module Make (E : Descriptor) = struct
  include E

  let http_request = Request.request

  let fetch_response ?timeout client req =
    Http_client.call
      ~method_name:E.method_name
      ?timeout
      ~weight:(Request.weight req)
      client
      (http_request req)
  ;;

  let ( let*? ) x f = Deferred.Result.bind ~f x

  let dispatch ?timeout client req =
    let*? resp = fetch_response ?timeout client req in
    Response.parse_response resp.status resp.body >>| Result.return
  ;;

  let dispatch_exn ?timeout client req =
    fetch_response ?timeout client req
    >>| Http_types.Response.Result.ok_exn ~here:[%here]
    >>= fun resp -> Response.parse_response resp.status resp.body
  ;;
end
