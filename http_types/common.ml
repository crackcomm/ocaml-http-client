(* SPDX-License-Identifier: BSD-3-Clause *)

open Core

module Status_code = struct
  type t = int [@@deriving sexp]

  let is_success (status : t) = Cohttp.Code.is_success status
end

module Method = struct
  type t =
    [ `GET
    | `POST
    | `HEAD
    | `DELETE
    | `PUT
    | `CONNECT
    | `OPTIONS
    | `TRACE
    | `Other of string
    ]
  [@@deriving sexp]
end

module Headers = struct
  type t = (string * string) list [@@deriving sexp]

  let add ~key ~value h : t = (key, value) :: h
  let replace ~key ~value h : t = Cohttp.Header.(replace (of_list h) key value |> to_list)
  let with_host headers host = add headers ~key:"Host" ~value:host

  let with_content_length headers len =
    replace headers ~key:"Content-Length" ~value:(Int.to_string len)
  ;;

  let with_host_of_uri headers url =
    match Uri.host url, Uri.port url with
    | Some h, Some p -> with_host headers (h ^ ":" ^ Int.to_string p)
    | Some h, None -> with_host headers h
    | _ -> headers
  ;;

  let rev = List.rev
end

module Query = struct
  module Param = struct
    type t = string * string list

    let of_kv ~key ~value = key, [ value ]
    let compare_key (x, _) (y, _) = String.compare x y
  end

  type t = Param.t list

  let empty = []
  let with_param q ~key ~value = q @ [ Param.of_kv ~key ~value ]
  let sort = List.sort ~compare:Param.compare_key
end
