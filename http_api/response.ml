(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Http_types

module type S = sig
  type t [@@deriving sexp_of]

  (* Parse a successful HTTP response into the desired data type. *)
  val parse_response : Status_code.t -> Body.t -> t Deferred.t
end

module Make (R : sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
end) : sig
  include S

  val of_string : string -> t
end
with type t = R.t = struct
  include R

  let parse_response _ body = Body.to_string' body >>| R.of_string
end

module Make_on_success (R : sig
  include S
  module Response_error : S
end) : S with type t = (R.t, R.Response_error.t) Result.t = struct
  type t = (R.t, R.Response_error.t) Result.t [@@deriving sexp_of]

  let parse_response status body =
    if Status_code.is_success status
    then R.parse_response status body >>| fun body -> Ok body
    else R.Response_error.parse_response status body >>| fun msg -> Error msg
  ;;
end
