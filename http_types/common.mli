(* SPDX-License-Identifier: BSD-3-Clause *)

module Status_code : sig
  type t = int [@@deriving sexp]

  (** [is_success code] alias for {!Cohttp.Code.is_success}. *)
  val is_success : t -> bool
end

module Method : sig
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

module Headers : sig
  (** The `t` type, represents a list of HTTP headers as a list of key-value pairs. *)
  type t = (string * string) list [@@deriving sexp]

  (** [add ~key ~value h] creates a new list of headers by adding a new header with the
      given key [key] and value [value] to the list of headers [h]. *)
  val add : key:string -> value:string -> t -> t

  (** [with_host headers host] creates a new list of headers by adding a "Host" header
      with the given value [host] to the list of headers [headers]. *)
  val with_host : t -> string -> t

  (** [with_content_length headers len] creates a new list of headers by adding a
      "Content-Length" header with the given value [len] to the list of headers [headers]. *)
  val with_content_length : t -> int -> t

  (** [with_host_of_uri headers url] creates a new list of headers by adding a "Host"
      header with the host and port from the given URL [url] to the list of headers
      [headers]. *)
  val with_host_of_uri : t -> Uri.t -> t

  (** [rev headers] reverses the order of the headers in the given list of headers
      [headers]. *)
  val rev : t -> t

  val replace : key:string -> value:string -> t -> t
end

module Query : sig
  module Param : sig
    (** Pair of [key] * [values]. *)
    type t = string * string list

    val of_kv : key:string -> value:string -> t
    val compare_key : t -> string * 'b -> int
  end

  (** The `t` type represents a list of query parameters as a list of key-value pairs. *)
  type t = Param.t list

  (** [empty] empty list of query parameters. *)
  val empty : t

  (** [with_param q ~key ~value] creates a new list of query parameters by adding a new
      key-value pair with the given key [key] and value [value] to the list of query
      parameters [q]. *)
  val with_param : t -> key:string -> value:string -> t

  (** [sort q] sorts the given list of query parameters [q] by the keys. *)
  val sort : t -> t
end
