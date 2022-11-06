(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Core
open Async

module T : sig
  type t

  val create : string -> t
  val host : t -> string
  val client : t -> Conn.t
end

module Outcome : sig
  (** Connection pool queue outcome type. *)
  type 'a t =
    (* Response type read by operation. *)
    | Response of 'a
    (* Returned solely on connection pool close. *)
    | Aborted
    (* Request timed out in-flight. *)
    | Timeout
    (* Request was rejected by rate limiter. *)
    | Queue_timeout
    (* Connection lost during request. *)
    | Connection_failure
    (* Fatal error. *)
    | Raised of exn
end

(** Weight rated limiter HTTP client connection pool. *)
type t

val of_host
  :  size:int
  -> burst_per_second:int
  -> sustained_rate_per_sec:float
  -> string
  -> t

val of_hosts : burst_per_second:int -> sustained_rate_per_sec:float -> string list -> t

val enqueue'
  :  weight:int
  -> t
  -> (T.t -> ('a, [< Conn.Error.t ]) Deferred.Result.t)
  -> 'a Outcome.t Deferred.t

val enqueue_timeout'
  :  weight:int
  -> timeout:Time_ns.Span.t
  -> t
  -> (timeout:Time_ns.Span.t -> T.t -> ('a, [< Conn.Error.t ]) Deferred.Result.t)
  -> 'a Outcome.t Deferred.t
