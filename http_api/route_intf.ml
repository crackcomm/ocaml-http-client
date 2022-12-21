(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async

module type Descriptor = sig
  module Request : Request.S
  module Response : Response.S

  (** Human readable method name. *)
  val method_name : string
end

module type S = sig
  include Descriptor

  (** [http_request req] constructs http request for {!Request.t}. *)
  val http_request : Request.t -> Http_types.Request.t

  (** [call ?timeout client req] constructs http request and calls using {!Http_client.t}. *)
  val call
    :  ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Request.t
    -> (Response.t, Http_types.Response.Error.t) Deferred.Result.t

  (** [dispatch_exn ?timeout client req] constructs http request and calls using {!Http_client.t}. *)
  val dispatch_exn
    :  ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Request.t
    -> Response.t Deferred.t
end

module type Route = sig
  module type Descriptor = Descriptor
  module type S = S

  module Make (E : Descriptor) :
    S with type Request.t := E.Request.t and type Response.t = E.Response.t
end
