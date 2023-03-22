(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
include Identifiable

(** Creates a function for generating new request IDs and starting new requests. The
    generated requests will have sequential IDs starting from 0. *)
val create : unit -> ?now:Time_ns.Span.t -> unit -> t

(** Finishes the given request and returns the finished request. This function expects a
    [Started] request, and will raise if called on a [Finished] request. *)
val finish_ : ?now:Time_ns.Span.t -> t -> t

(** Returns unique ID of the request. *)
val id : t -> int

(** Returns the duration of the given request as a [Time_ns.Span.t], or [None] if the
    request is not finished. *)
val span : t -> Time_ns.Span.t option

(** Returns the duration of a finished request ID. *)
val span_exn : t -> Time_ns.Span.t
