(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Common

(** Common response type returned by backends. *)
type t =
  { status : Status_code.t
  ; headers : Headers.t
  ; body : Body.t
  }
[@@deriving sexp_of]

let status { status; _ } = status

module Error = struct
  (** Client call error type. *)
  type t =
    [ `Connection_failure
    | `Raised of exn
    | `Timeout
    | (* NOTE: for the reason of simplicity those are included here *)
      `Queue_timeout
    ]
  [@@deriving sexp_of]

  let failwith ~here err = failwiths ~here "Result.ok_exn" err sexp_of_t
end

module Result = struct
  type nonrec t = (t, Error.t) Result.t [@@deriving sexp_of]

  let ok_exn ~here (t : t) =
    match t with
    | Ok v -> v
    | Error err -> Error.failwith ~here err
  ;;
end
