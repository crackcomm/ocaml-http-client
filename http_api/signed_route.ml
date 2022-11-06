(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Async
open Core
include Signed_route_intf

module Make (E : Descriptor) :
  S
    with type Request.t := E.Request.t
     and type Response.t := E.Response.t
     and type Signer.t := E.Signer.t = struct
  module Signer = E.Signer
  module R = Make_route.Make (E)
  include R
  open Route_internal

  let next_req_id = Request_id.create ()
  let method_str = Httpaf.Method.to_string Request.method_

  let request_signed_body ~req_id ~method_ ~path ?query creds ~timeout conn =
    Http_client.connected (Http_client.Pool.T.client conn)
    >>= fun { r; w; _ } ->
    [%log.global.debug
      "starting request"
        (req_id : Request_id.t)
        (Request.path : string)
        (method_str : string)];
    let host = Http_client.Pool.T.host conn in
    (* NOTE: Signer takes timeout of the request, some exchanges accept it. *)
    let req, body = Signer.signed_request ~timeout ?query ~method_ ~host ~path creds in
    Http_client.request ~timeout ?body req r w
  ;;

  let request ?(timeout = Time_ns.Span.of_sec 5.0) signer pool req =
    let req_id = next_req_id () in
    let weight = Weight.of_req Request.weight req in
    request_signed_body
      ~req_id
      ~method_:Request.method_
      ~path:Request.path
      ?query:(Request.query req)
      signer
    |> Http_client.Pool.enqueue_timeout' ~weight ~timeout pool
    >>| outcome_of_response ~req_id (module E)
  ;;

  let fetch_exn signer pool req =
    let fetch () = request signer pool req in
    Route_internal.fetch_exn fetch
  ;;
end
