(* SPDX-License-Identifier: BSD-3-Clause *)

open Core

module Weight = struct
  type 'a t =
    | Static of int
    | Of_request of ('a -> int)

  let of_req t req =
    match t with
    | Static w -> w
    | Of_request f -> f req
  ;;
end

(* At first we will fail on nearly every error so this is fine. *)
module Error_message = struct
  type t =
    { code : int
    ; message : string
    }
end

module Outcome = struct
  type 'a t =
    | Success of 'a
    | Error_message of Error_message.t
    | Connection_failure
    | Timeout
    | Queue_timeout

  let success_exn = function
    | Success r -> r
    | Error_message e -> failwith (sprintf "error code %d message: %s" e.code e.message)
    | Timeout -> failwith "result_exn on Timeout"
    | Queue_timeout -> failwith "result_exn on Queue_timeout"
    | Connection_failure -> failwith "result_exn on Connection_failure"
  ;;

  let result_or_exn = function
    | Success r -> Ok r
    | Error_message e -> Error e
    | Timeout -> failwith "result_exn on Timeout"
    | Queue_timeout -> failwith "result_exn on Queue_timeout"
    | Connection_failure -> failwith "result_exn on Connection_failure"
  ;;
end
