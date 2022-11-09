(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
open Async_uri
open Common

include module type of struct
  include Async_uri.Persistent
end

(** [create ~server_name ?on_event ?reconnect_delay url] Crates a persistent connection to
    [url]. *)
val create
  :  server_name:string
  -> ?on_event:(Uri.t Persistent.Event.t -> unit Deferred.t)
  -> ?reconnect_delay:(unit -> Time_ns.Span.t)
  -> Uri.t
  -> Persistent.t

module Request : sig
  val create
    :  ?headers:Headers.t
    -> ?query:Query.t
    -> method_:Httpaf.Method.t
    -> host:string
    -> path:string
    -> unit
    -> Httpaf.Request.t
end

module Response : sig
  type t =
    { status : Httpaf.Status.t
    ; headers : Httpaf.Headers.t
    ; body : Bigstring.t
    }
end

module Error : sig
  type t =
    [ `Raised of exn
    | `Timeout
    | `Connection_failure
    ]
  [@@deriving sexp_of]
end

(** [request ?timeout ?body url r w] Sends a HTTP request to [url] to [w] and reads
    response from [r]. *)
val request
  :  ?timeout:Time_ns.Span.t
  -> ?body:string
  -> Httpaf.Request.t
  -> Reader.t
  -> Writer.t
  -> (Response.t, Error.t) Deferred.Result.t
