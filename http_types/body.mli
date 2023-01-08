(* SPDX-License-Identifier: BSD-3-Clause *)

open Async_kernel

module How : sig
  type t =
    [ `Ignore
    | `String
    | `Strings
    | `Pipe
    ]
  [@@deriving sexp_of]
end

type t =
  [ `Empty
  | `String of string
  | `Strings of string list
  | `Pipe of string Pipe.Reader.t
  ]
[@@deriving sexp_of]

(** [to_string_opt' t] converts the given [t] value to a string, if possible. *)
val to_string_opt' : t -> string option Deferred.t

(** [to_string' t] converts the given [t] value to a string, if possible. *)
val to_string' : t -> string Deferred.t

(** [to_string_list' t] converts the given [t] value to a list of strings. *)
val to_string_list' : t -> string list Deferred.t

(** [of_string str] creates a new [t] value from the given string. *)
val of_string : string -> t

(** [of_string_list str_list] creates a new [t] value from the given list of strings. *)
val of_string_list : string list -> t

(** [length_opt data] returns length of [data] or [None] if [data] is a pipe. *)
val length_opt : t -> int option

(** [is_pipe data] returns true if data is of type [`Pipe]. *)
val is_pipe : t -> bool

(** [close data] closes the [Pipe.Reader.t]. *)
val close : t -> unit
