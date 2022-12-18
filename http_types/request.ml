(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Common

type t =
  { uri : Uri_sexp.t
  ; meth : Method.t
  ; headers : Headers.t
  ; chunked : bool
  ; body : Body.t
  ; body_length : int64 option
  }
[@@deriving sexp_of]

let path { uri; _ } = Uri.path uri

let make ?(headers = []) ?(body = `Empty) ?body_length meth uri =
  let chunked = Body.is_pipe body in
  let body_length =
    match body_length with
    | Some _ as v -> v
    | None -> Body.length_opt body |> Option.map ~f:Int64.of_int
  in
  { uri; meth; headers; chunked; body; body_length }
;;

(** [with_headers req h] extends request headers with [h] *)
let with_headers (req : t) h = { req with headers = req.headers @ h }
