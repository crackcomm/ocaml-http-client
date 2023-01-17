(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Http_types

module type S = sig
  module Error : sig
    type t [@@deriving sexp_of]
  end

  (** Response type. *)
  type t [@@deriving sexp_of]

  (* Parse a successful HTTP response into the desired data type. *)
  val parse_response : Status_code.t -> Body.t -> (t, Error.t) Deferred.Result.t
end

module type Of_string_with_status = sig
  type t [@@deriving sexp_of]

  val of_string : status:int -> string -> t
end

module type Of_string_with_error = sig
  module Error : Of_string_with_status

  type t [@@deriving sexp_of]

  val of_string : string -> t
end
