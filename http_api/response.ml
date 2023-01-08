(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Http_types

module type S = sig
  module Error : sig
    (** Error type. *)
    type t [@@deriving sexp_of]
  end

  (** Response type. *)
  type t [@@deriving sexp_of]

  (* Parse a successful HTTP response into the desired data type. *)
  val parse_response : Status_code.t -> Body.t -> (t, Error.t) Deferred.Result.t
end

module Of_string = struct
  module type I = sig
    type t [@@deriving sexp_of]

    val of_string : string -> t
  end

  module Make (R : I) : S with type t = R.t and module Error = Base.Unit = struct
    include R
    module Error = Unit

    let parse_response _ body = Body.to_string' body >>| fun resp -> Ok (R.of_string resp)
  end

  module type I_with_error = sig
    module Error : I

    type t [@@deriving sexp_of]

    val of_string : string -> t
  end

  module Make_on_success (R : I_with_error) :
    S with type t = R.t and module Error = R.Error = struct
    include R

    let parse_response status body =
      Body.to_string' body
      >>| fun resp ->
      if Status_code.is_success status
      then Ok (R.of_string resp)
      else Error (R.Error.of_string resp)
    ;;
  end
end
