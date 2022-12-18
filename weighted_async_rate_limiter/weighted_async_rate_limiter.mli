(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
open Limiter_async

module Outcome : sig
  type 'a t =
    (* Response type read by operation. *)
    | Response of 'a
    (* Returned by resoure throttle. *)
    | Aborted
    (* Request was rejected by rate limiter. *)
    | Queue_timeout
    (* Fatal error. *)
    | Raised of exn
end

(** Weight rated limiter HTTP client connection pool. *)
type 'a t =
  { pool : 'a Resource_throttle.t
  ; token_bucket : Token_bucket.t
  }

val enqueue'
  :  weight:int
  -> 'a t
  -> ('a -> 'response Deferred.t)
  -> 'response Outcome.t Deferred.t

val enqueue_timeout'
  :  weight:int
  -> timeout:Time_ns.Span.t
  -> 'a t
  -> (?timeout:Time_ns.Span.t -> 'a -> 'response Deferred.t)
  -> 'response Outcome.t Deferred.t
