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

  module Error : sig
    type t =
      [ `Message of Response.Error.t
      | Http_types.Error.t
      ]
    [@@deriving sexp_of]
  end

  (** [http_request req] constructs http request for {!Request.t}. *)
  val http_request : Request.t -> Http_types.Request.t

  (** [dispatch ?timeout client req] constructs http request and dispatches using
      {!Http_client.t}. *)
  val dispatch
    :  ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Request.t
    -> (Response.t, Error.t) Result.t Deferred.t

  (** [dispatch_exn ?timeout client req] constructs http request and dispatches using
      {!Http_client.t}. *)
  val dispatch_exn
    :  ?here:Lexing.position
    -> ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Request.t
    -> Response.t Deferred.t
end

module type Route = sig
  module type Descriptor = Descriptor
  module type S = S

  module Make (E : Descriptor) :
    S
      with type Request.t = E.Request.t
       and type Response.t = E.Response.t
       and type Response.Error.t = E.Response.Error.t
end
