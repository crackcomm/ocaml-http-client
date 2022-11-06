(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Async
open Core
open Limiter_async

module T = struct
  type t =
    { client : Conn.t
    ; host : string
    }

  let create host =
    let url = Uri.make ~scheme:"https" ~host () in
    let client = Conn.create ~server_name:host url in
    { client; host }
  ;;

  let host t = t.host
  let client t = t.client
end

module Pool = struct
  include Resource_throttle

  type t = T.t Resource_throttle.t

  let of_hosts hosts : t =
    let resources = List.map ~f:(fun h -> T.create h) hosts in
    create_exn ~resources ~continue_on_error:true ()
  ;;

  let of_host ~size url = List.init ~f:(fun _ -> url) size |> of_hosts
end

(** Request weight token bucket. *)
module Token_bucket = struct
  include Token_bucket

  let create_exn ~burst_per_second:burst_size ~sustained_rate_per_sec () =
    create_exn
      ~burst_size
      ~initial_burst_size:0
      ~sustained_rate_per_sec
      ~continue_on_error:true
      ()
  ;;
end

module Outcome = struct
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

(** Weight rate limited HTTP client connection pool. *)
type t =
  { pool : Pool.t
  ; token_bucket : Token_bucket.t
  }

let of_host ~size ~burst_per_second ~sustained_rate_per_sec host =
  { pool = Pool.of_host ~size host
  ; token_bucket = Token_bucket.create_exn ~burst_per_second ~sustained_rate_per_sec ()
  }
;;

let of_hosts ~burst_per_second ~sustained_rate_per_sec hosts =
  { pool = Pool.of_hosts hosts
  ; token_bucket = Token_bucket.create_exn ~burst_per_second ~sustained_rate_per_sec ()
  }
;;

let enqueue' ~weight t f =
  Token_bucket.enqueue' t.token_bucket weight (fun _ -> Pool.enqueue' t.pool f) ()
  >>| function
  | Limiter_async.Outcome.Ok Limiter_async.Outcome.Aborted -> Outcome.Aborted
  | Ok (Ok out) ->
    (match out with
    | Error (`Raised e) -> Outcome.Raised e
    | Error `Timeout -> Outcome.Timeout
    | Error `Connection_failure -> Outcome.Connection_failure
    | Ok r -> Outcome.Response r)
  | Ok (Raised e) -> Outcome.Raised e
  | Aborted -> Aborted
  | Raised e -> Raised e
;;

let enqueue_timeout' ~weight ~timeout t f =
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
