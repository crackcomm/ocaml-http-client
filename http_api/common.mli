(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Core

module Weight : sig
  type 'a t =
    | Static of int
    | Of_request of ('a -> int)

  val of_req : 'a t -> 'a -> int
end

module Error_message : sig
  type t =
    { code : int
    ; message : string
    }
end

module Outcome : sig
  type 'a t =
    | Success of 'a
    | Error_message of Error_message.t
    | Connection_failure
    | Timeout
    | Queue_timeout

  val success_exn : 'a t -> 'a
  val result_or_exn : 'a t -> ('a, Error_message.t) Result.t
end
