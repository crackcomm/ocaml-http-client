(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
include Common

module type Signer = sig
  type t

  val signed_request
    :  ?timeout:Time_ns.Span.t
    -> ?query:Http_client.Query.t
    -> method_:Httpaf.Method.t
    -> host:string
    -> path:string
    -> t
    -> Httpaf.Request.t * string option
end

module type Descriptor = sig
  include Make_route_intf.Descriptor
  module Signer : Signer
end

module type S = sig
  include Descriptor

  module Signer : sig
    type t
  end

  val request
    :  ?timeout:Time_ns.Span.t
    -> Signer.t
    -> Http_client.Pool.t
    -> Request.t
    -> Response.t Outcome.t Deferred.t

  (** Fetch in infinite loop. Used only in pathological cases. *)
  val fetch_exn : Signer.t -> Http_client.Pool.t -> Request.t -> Response.t Deferred.t
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
