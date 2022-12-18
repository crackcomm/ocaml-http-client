(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
open Cohttp_async

let log_name = "cohttp"

module Conn = struct
  include Client.Connection

  (* HACK: no [close_finished] for cohttp connection. *)
  let close_finished (conn : t) =
    let closed = Ivar.create () in
    Clock.run_at_intervals
      ~stop:(Ivar.read closed)
      Time.Span.(of_sec 1.)
      (fun () -> if is_closed conn then Ivar.fill closed () else ());
    Ivar.read closed
  ;;

  let connect uri = connect uri
end

let%test_unit "Conn : Http_backend_intf.Client.S_conn" =
  ignore (module Conn : Http_backend_intf.Client.S_conn)
;;

module Persistent = Persistent_connection_kernel.Make (Conn)

let call ?timeout (conn : Conn.t) (req : Http_types.Request.t)
    : Http_types.Response.Result.t Deferred.t
  =
  let body : Cohttp_async.Body.t =
    match req.body with
    | `Empty -> `Empty
    | `String s -> `String s
    | `Strings s -> `Strings s
    | `Pipe_string p -> `Pipe p
    | `Bigstring s -> `String (Bigstring.to_string s)
    | `Bigstrings s -> `Strings (List.map ~f:Bigstring.to_string s)
    | `Pipe_bigstring pipe ->
      let r, w = Pipe.create () in
      don't_wait_for (Pipe.transfer pipe w ~f:Bigstring.to_string);
      don't_wait_for (Pipe.closed pipe >>| fun () -> Pipe.close w);
      `Pipe r
  in
  let req =
    Cohttp_async.Request.make_for_client
      ~headers:(Cohttp.Header.of_list req.headers)
      ~chunked:req.chunked
      ?body_length:req.body_length
      (req.meth :> Cohttp.Code.meth)
      req.uri
  in
  let response = Conn.request ~body conn req >>| Result.return in
  let timeout =
    match timeout with
    | None -> Deferred.never ()
    | Some timeout -> Clock_ns.after timeout >>| fun () -> Error `Timeout
  in
  Deferred.any [ response; timeout ]
  >>| fun res ->
  match res with
  | Ok (res, body) ->
    let body =
      match body with
      | `Empty -> `Empty
      | `String s -> `String s
      | `Strings s -> `Strings s
      | `Pipe pipe -> `Pipe_string pipe
    in
    Ok
      Http_types.Response.
        { status = Cohttp.Code.code_of_status res.status
        ; headers = Cohttp.Header.to_list res.headers
        ; body
        }
  | Error `Timeout -> Error `Timeout
  | Error e -> Error e
;;
