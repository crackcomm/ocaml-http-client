(* SPDX-License-Identifier: BSD-3-Clause *)

open Core

module T = struct
  type t =
    { code : int
    ; message : string
    }
  [@@deriving sexp]
end

include T

module Result = struct
  type 'a t = ('a, T.t) result

  let value_exn ~here : 'a t -> 'a = function
    | Ok resp -> resp
    | Error e -> failwiths ~here "Http_api_message.Result.value_exn" e T.sexp_of_t
  ;;
end

let of_string ~status message = { code = status; message }
