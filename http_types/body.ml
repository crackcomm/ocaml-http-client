(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async

(** Used to suggest to the client expected response type. *)
module How = struct
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

let to_string_opt' : t -> string option Deferred.t = function
  | `Empty -> return None
  | `String s -> return (Some s)
  | `Strings s -> return (Some (String.concat s))
  | `Pipe pipe -> Pipe.to_list pipe >>| String.concat >>| Option.return
;;

let to_string' t = to_string_opt' t >>| Option.value ~default:""

let to_string_list' = function
  | `Empty -> return []
  | `String s -> return [ s ]
  | `Strings s -> return s
  | `Pipe pipe -> Pipe.to_list pipe
;;

let of_string s : t = `String s
let of_string_list s : t = `Strings s

(** [length_opt data] returns length of [data] or [None] if [data] is a pipe. *)
let length_opt = function
  | `Empty -> None
  | `String s -> Some (String.length s)
  | `Strings s -> Some (List.sum (module Int) s ~f:String.length)
  | `Pipe _ -> None
;;

let%test "string length_opt" = Stdlib.(length_opt (of_string "test") = Some 4)

let close = function
  | `Pipe pipe -> Pipe.close_read pipe
  | _ -> ()
;;

let is_pipe = function
  | `Pipe _ -> true
  | _ -> false
;;
