(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Http_types

module type Signer = sig
  type t

  val sign_request : t -> Request.t -> Request.t
end

module type Descriptor = sig
  include Route_intf.Descriptor
  module Signer : Signer
end

module type S = sig
  include Descriptor

  module Signer : sig
    type t
  end

  (** [http_request req] constructs a signed http request for {!Request.t}. *)
  val http_request : Signer.t -> Request.t -> Http_types.Request.t

  (** [call ?timeout client signer req] constructs a signed http request and calls using
      {!Http_client.t}. *)
  val call
    :  ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Signer.t
    -> Request.t
    -> (Response.t, Http_types.Response.Error.t) Deferred.Result.t

  (** [call_exn ?timeout client signer req] constructs a signed http request and calls
      using {!Http_client.t}. *)
  val call_exn
    :  ?timeout:Time_ns.Span.t
    -> Http_client.t
    -> Signer.t
    -> Request.t
    -> Response.t Deferred.t
end

module type Signed_route = sig
  module type Signer = Signer
  module type Descriptor = Descriptor
  module type S = S

  module Make (E : Descriptor) :
    S
      with type Request.t := E.Request.t
       and type Response.t := E.Response.t
       and type Signer.t := E.Signer.t
end
