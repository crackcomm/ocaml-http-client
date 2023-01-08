(* SPDX-License-Identifier: BSD-3-Clause *)

open Core

(** Client call error type. *)
type t =
  [ `Connection_failure
  | `Raised of exn
  | `Timeout
  | `Queue_timeout
  ]
[@@deriving sexp_of]

let failwith ~here err = failwiths ~here "Http_types.Error.failwith" err sexp_of_t
