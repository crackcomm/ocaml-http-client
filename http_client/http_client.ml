(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
open Http_types
module Pool = Pool

type call =
  ?method_name:string
  -> ?timeout:Core.Time_ns.Span.t
  -> ?weight:int
  -> Request.t
  -> Response.Result.t Deferred.t

type t = { call : call }

let outcome_result_exn (resp : Response.Result.t Weighted_async_rate_limiter.Outcome.t)
    : Response.Result.t
  =
  match resp with
  | Weighted_async_rate_limiter.Outcome.Response response -> response
  | Queue_timeout -> Error `Queue_timeout
  | Aborted -> failwith "connection pool was killed"
  | Raised e -> Error (`Raised e)
;;

let set_default_https uri =
  match Uri.port uri, Uri.scheme uri with
  | None, Some "http" -> Uri.with_port uri (Some 80)
  | None, (None | Some "") ->
    Uri.with_scheme (Uri.with_port uri (Some 443)) (Some "https")
  | None, (Some "wss" | Some "https") -> Uri.with_port uri (Some 443)
  | None, Some _ -> uri
  | Some _, _ -> uri
;;

let create_persistent
    (module Client : Http_backend_intf.Client.S_persistent)
    ?(continue_on_error = true)
    ?reconnect_delay
    ~burst_per_second:burst_size
    ~sustained_rate_per_sec
    uris
  =
  let module Pool = Pool.Make (Client) in
  let uris = List.map uris ~f:set_default_https in
  let pool =
    Pool.of_uris
      ~continue_on_error
      ~burst_per_second:burst_size
      ~sustained_rate_per_sec
      ?reconnect_delay
      uris
  in
  let call_client ~req_id ~method_name (request : Request.t) ?timeout conn =
    let host = Client.Persistent.server_name conn in
    Client.Persistent.connected conn
    >>= fun conn ->
    let uri = Uri.with_host request.uri (Some host) in
    let headers = Headers.with_host request.headers host in
    let request = Request.{ request with uri; headers } in
    [%log.global.debug
      "starting request"
        (method_name : string)
        (req_id : Request_id.t)
        (host : string)
        (request.headers : Headers.t)];
    Client.call ?timeout conn request
  in
  let enqueue ~req_id ~method_name ?timeout ?(weight = 1) (request : Request.t) =
    let call = call_client ~req_id ~method_name request in
    match timeout with
    | None -> Weighted_async_rate_limiter.enqueue' ~weight pool call
    | Some timeout ->
      Weighted_async_rate_limiter.enqueue_timeout' ~weight ~timeout pool call
  in
  let next_req_id = Request_id.create () in
  let call ?(method_name = "call") ?timeout ?weight req =
    let req_id = next_req_id () in
    enqueue ~req_id ~method_name ?timeout ?weight req >>| outcome_result_exn
  in
  { call }
;;

let create
    (module Client : Http_backend_intf.Client.S)
    ?(continue_on_error = true)
    ~burst_per_second:burst_size
    ~sustained_rate_per_sec
    ~max_connections
    uri
  =
  let uri = set_default_https uri in
  let uri = Uri.with_scheme uri (Some "https") in
  let client () =
    [%log.global.debug "Http_client connecting." (uri : Uri_sexp.t)];
    Client.Conn.connect uri
  in
  let resources = List.init max_connections ~f:(fun _ -> client) in
  let pool =
    Limiter_async.Resource_throttle.create_exn
      ~resources
      ~burst_size
      ~sustained_rate_per_sec
      ~continue_on_error
      ()
  in
  let pool =
    Weighted_async_rate_limiter.
      { pool
      ; token_bucket =
          Limiter_async.Token_bucket.create_exn
            ~burst_size
            ~sustained_rate_per_sec
            ~continue_on_error:true
            ()
      }
  in
  let host =
    match Uri.host uri with
    | Some "" | None -> None
    | Some host -> Some host
  in
  let call_client ~req_id ~method_name (request : Request.t) ?timeout conn =
    conn ()
    >>= fun conn ->
    let headers, uri =
      match host with
      | Some host as v ->
        Headers.with_host request.headers host, Uri.with_host request.uri v
      | None ->
        (match Uri.host request.uri with
        | Some host -> Headers.with_host request.headers host, request.uri
        | None -> request.headers, request.uri)
    in
    let request = Request.{ request with uri; headers } in
    [%log.global.debug
      "Http_client.create call"
        (method_name : string)
        (req_id : Request_id.t)
        (uri : Uri_sexp.t)
        (request.headers : Headers.t)];
    Client.call ?timeout conn request
  in
  let enqueue ~req_id ~method_name ?timeout ?(weight = 1) (request : Request.t) =
    let call = call_client ~req_id ~method_name request in
    match timeout with
    | None -> Weighted_async_rate_limiter.enqueue' ~weight pool call
    | Some timeout ->
      Weighted_async_rate_limiter.enqueue_timeout' ~weight ~timeout pool call
  in
  let next_req_id = Request_id.create () in
  let call ?(method_name = "call") ?timeout ?weight req =
    let req_id = next_req_id () in
    enqueue ~req_id ~method_name ?timeout ?weight req >>| outcome_result_exn
  in
  { call }
;;

let create_test (module Client : Http_backend_intf.Client.S) =
  create
    (module Client)
    ~continue_on_error:false
    ~burst_per_second:5
    ~sustained_rate_per_sec:1.
    ~max_connections:1
;;

let call ?method_name ?timeout ?weight client request =
  client.call ?method_name ?timeout ?weight request
;;
