(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
open Limiter_async
module Outcome = Weighted_limiter_async.Outcome

module type S = sig
  module Persistent : Persistent_connection_kernel.S

  (** Pool type. *)
  type t = Persistent.t Weighted_limiter_async.t

  val of_uris
    :  continue_on_error:bool
    -> ?reconnect_delay:(unit -> Time_ns.Span.t)
    -> burst_per_second:int
    -> sustained_rate_per_sec:float
    -> Uri.t list
    -> t
end

module Make (C : Http_backend_intf.Client.S_persistent) :
  S with module Persistent = C.Persistent = struct
  include C
  module Outcome = Weighted_limiter_async.Outcome

  type t = Persistent.t Weighted_limiter_async.t

  let of_uris
      ~continue_on_error
      ?reconnect_delay
      ~burst_per_second:burst_size
      ~sustained_rate_per_sec
      urls
    =
    let on_event ev =
      [%log.global.debug
        "Http_client.Pool.on_event." (ev : Uri_sexp.t Persistent.Event.t)];
      Deferred.unit
    in
    let create ~server_name ?(on_event = on_event) ?reconnect_delay:retry_delay url =
      let connect addr = Deferred.Or_error.try_with (fun _ -> C.Conn.connect addr) in
      let address () = Deferred.Or_error.return url in
      Persistent.create
        ~on_event
        ?retry_delay
        ~connect
        ~server_name
        ~address:(module Uri_sexp)
        address
    in
    let pool =
      let resources =
        List.map
          ~f:(fun url ->
            create
              ?reconnect_delay
              ~server_name:(Uri.host url |> Option.value_exn ~here:[%here])
              url)
          urls
      in
      Resource_throttle.create_exn ~resources ~continue_on_error ()
    in
    Weighted_limiter_async.
      { pool
      ; token_bucket =
          Token_bucket.create_exn
            ~burst_size
            ~sustained_rate_per_sec
            ~continue_on_error:true
            ()
      }
  ;;
end
