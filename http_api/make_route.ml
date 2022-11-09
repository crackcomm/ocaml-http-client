(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
include Common
include Make_route_intf
include Route_internal

module Make (E : Descriptor) :
  S with type Request.t := E.Request.t and type Response.t := E.Response.t = struct
  include E

  let next_req_id = Request_id.create ()

  let send_request ~req_id ~method_ ~path ?query ~timeout conn =
    Http_client.connected (Http_client.Pool.T.client conn)
    >>= fun { r; w; _ } ->
    let host = Http_client.Pool.T.host conn in
    [%log.global.debug
      "starting request" (E.method_name : string) (req_id : Request_id.t) (host : string)];
    let req = Http_client.Request.create ?query ~method_ ~host ~path () in
    Http_client.request ~timeout req r w
  ;;

  let request ?(timeout = Time_ns.Span.of_sec 5.0) pool req =
    let req_id = next_req_id () in
    let weight = Weight.of_req Request.weight req in
    send_request
      ~req_id
      ~method_:Request.method_
      ~path:Request.path
      ?query:(Request.query req)
    |> Http_client.Pool.enqueue_timeout' ~weight ~timeout pool
    >>| Route_internal.outcome_of_response ~req_id (module E)
  ;;

  let fetch_exn pool req =
    let fetch () = request pool req in
    Route_internal.fetch_exn fetch
  ;;
end
