(* SPDX-License-Identifier: BSD-3-Clause *)

open Common

(** Common response type returned by backends. *)
type t =
  { status : Status_code.t
  ; headers : Headers.t
  ; body : Body.t
  }
[@@deriving sexp_of]

let status { status; _ } = status
