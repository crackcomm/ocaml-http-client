(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Limiter_async

module Outcome = struct
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

type 'a t =
  { pool : 'a Resource_throttle.t
  ; token_bucket : Token_bucket.t
  }

let enqueue' ~weight t f =
  Token_bucket.enqueue'
    t.token_bucket
    weight
    (fun _ -> Resource_throttle.enqueue' t.pool f)
    ()
  >>| function
  | Ok (Ok r) -> Outcome.Response r
  | Aborted -> Outcome.Aborted
  | Ok Aborted -> Outcome.Aborted
  | Raised e -> Raised e
  | Ok (Raised e) -> Outcome.Raised e
;;

let enqueue_timeout'
  ~weight
  ~timeout
  t
  (f : ?timeout:Time_ns.Span.t -> 'a -> 'b Deferred.t)
  =
  let start = Time_ns.now () in
  let l = Token_bucket.to_limiter t.token_bucket |> Expert.to_jane_limiter in
  let open Limiter in
  let _ = in_bucket ~now:start l in
  match Expert.tokens_may_be_available_when ~now:start l weight with
  | Tokens_may_be_available_result.At s ->
    let when_ = Time_ns.diff s start in
    let timeout = Time_ns.Span.(timeout - when_) in
    if Time_ns.Span.is_negative timeout
    then Deferred.return Outcome.Queue_timeout
    else enqueue' ~weight t (f ~timeout)
  | When_return_to_hopper_is_called ->
    (* Call with unchanged timeout. *)
    enqueue' ~weight t (f ~timeout)
  | Never_because_greater_than_bucket_limit ->
    failwith "enqueued with weight greater than limit"
;;

(* Test for request weight token bucket. *)
let%test _ =
  let l =
    Limiter.Token_bucket.create_exn
      ~now:(Time_ns.now ())
      ~burst_size:300
      ~sustained_rate_per_sec:(1200.0 /. 60.0)
      ()
  in
  let start = Time_ns.now () in
  let end_ = Time_ns.add start (Time_ns.Span.of_min 60.0) in
  let rec take ~now t sum =
    match Limiter.Token_bucket.try_take l t ~now with
    | Limiter.Try_take_result.Taken -> take ~now t (sum + t)
    | Unable ->
      let now = Time_ns.add now (Time_ns.Span.of_ms 300.) in
      if Time_ns.is_earlier ~than:end_ now then take ~now t sum else sum
    | Asked_for_more_than_bucket_limit -> sum
  in
  take ~now:start 30 0 <= 72000
;;
