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
      Time.Span.(of_ms 250.)
      (fun () -> if is_closed conn then Ivar.fill closed ());
    Ivar.read closed
  ;;

  let connect uri = connect uri
end

module Persistent = Persistent_connection_kernel.Make (Conn)

let call ?timeout (conn : Conn.t) (req : Http_types.Request.t)
  : (Http_types.Response.t, Http_types.Error.t) Result.t Deferred.t
  =
  let request =
    Cohttp_async.Request.make_for_client
      ~headers:(Cohttp.Header.of_list req.headers)
      ~chunked:req.chunked
      ?body_length:req.body_length
      (req.meth :> Cohttp.Code.meth)
      req.uri
  in
  let response = Conn.request ~body:req.body conn request >>| Result.return in
  let timeout =
    match timeout with
    | None -> Deferred.never ()
    | Some timeout -> Clock_ns.after timeout >>| fun () -> Error `Timeout
  in
  Deferred.any [ response; timeout ]
  >>| fun res ->
  match res with
  | Ok (res, body) ->
    Ok
      Http_types.Response.
        { status = Cohttp.Code.code_of_status res.status
        ; headers = Cohttp.Header.to_list res.headers
        ; body
        }
  | Error `Timeout -> Error `Timeout
  | Error e -> Error e
;;
