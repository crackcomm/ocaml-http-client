(* SPDX-License-Identifier: BSD-3-Clause *)

open Http_types
open Async_kernel

module type S_conn = sig
  type t

  val connect : Uri.t -> t Deferred.t
end

module type S = sig
  (* log_name of the backend for logs *)
  val log_name : string

  module Conn : S_conn

  val call
    :  ?timeout:Core.Time_ns.Span.t
    -> Conn.t
    -> Request.t
    -> Response.Result.t Deferred.t
end

module type S_persistent = sig
  include S
  module Persistent : Persistent_connection_kernel.S with type conn = Conn.t
end
