(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Http_types

module type Signer = sig
  type t

  val sign_request : t -> Request.t -> Request.t
end

module type S = sig
  include Route_intf.Descriptor

  module Signer : sig
    type t
  end

  module Error : sig
    type t =
      [ `Message of Response.Error.t
      | Http_types.Error.t
      ]
    [@@deriving sexp_of]
  end

  (** [http_request req] constructs a signed http request for {!Request.t}. *)
  val http_request : Signer.t -> Request.t -> Http_types.Request.t

  (** [dispatch ?timeout client signer req] constructs a signed http request and
      dispatches using {!Http_client.t}. *)
  val dispatch
    :  ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Signer.t
    -> Request.t
    -> (Response.t, Error.t) Result.t Deferred.t

  (** [dispatch_exn ?timeout client signer req] constructs a signed http request and
      dispatches using {!Http_client.t}. *)
  val dispatch_exn
    :  ?here:Lexing.position
    -> ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Signer.t
    -> Request.t
    -> Response.t Deferred.t
end

module type Signed_route = sig
  module type Signer = Signer
  module type S = S

  module Make (C : Signer) (E : Route_intf.Descriptor) :
    S
      with type Request.t = E.Request.t
       and type Response.t = E.Response.t
       and type Response.Error.t = E.Response.Error.t
       and type Signer.t := C.t
end
