(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async

(** Used to suggest to the client expected response type. *)
module How = struct
  type t =
    [ `Ignore
    | `String
    | `Strings
    | `Bigstring
    | `Bigstrings
    | `Pipe_string
    | `Pipe_bigstring
    ]
  [@@deriving sexp_of]
end

type t =
  [ `Empty
  | `String of string
  | `Strings of string list
  | `Bigstring of Bigstring.t
  | `Bigstrings of Bigstring.t list
  | `Pipe_string of string Pipe.Reader.t
  | `Pipe_bigstring of Bigstring.t Pipe.Reader.t
  ]
[@@deriving sexp_of]

let to_string_opt' : t -> string option Deferred.t = function
  | `Empty -> return None
  | `String s -> return (Some s)
  | `Strings s -> return (Some (String.concat s))
  | `Bigstring s -> return (Some (Bigstring.to_string s))
  | `Bigstrings s -> return (Some (List.map ~f:Bigstring.to_string s |> String.concat))
  | `Pipe_string pipe -> Pipe.to_list pipe >>| String.concat >>| Option.return
  | `Pipe_bigstring pipe ->
    Pipe.to_list pipe
    >>| fun s -> List.map ~f:Bigstring.to_string s |> String.concat |> Option.return
;;

let to_string' t = to_string_opt' t >>| Option.value ~default:""

let to_string_list' = function
  | `Empty -> return []
  | `String s -> return [ s ]
  | `Strings s -> return s
  | `Bigstring s -> return [ Bigstring.to_string s ]
  | `Bigstrings s -> return (List.map ~f:Bigstring.to_string s)
  | `Pipe_string pipe -> Pipe.to_list pipe
  | `Pipe_bigstring pipe -> Pipe.to_list pipe >>| List.map ~f:Bigstring.to_string
;;

let of_string s : t = `String s
let of_string_list s : t = `Strings s
let of_bigstring s : t = `Bigstring s
let of_bigstring_list s : t = `Bigstrings s

(** [length_opt data] returns length of [data] or [None] if [data] is a pipe. *)
let length_opt = function
  | `Empty -> None
  | `String s -> Some (String.length s)
  | `Strings s -> Some (List.sum (module Int) s ~f:String.length)
  | `Bigstring s -> Some (Bigstring.length s)
  | `Bigstrings s -> Some (List.sum (module Int) s ~f:Bigstring.length)
  | `Pipe_string _ | `Pipe_bigstring _ -> None
;;

let%test "string length_opt" = Stdlib.(length_opt (of_string "test") = Some 4)

let close = function
  | `Pipe_string pipe -> Pipe.close_read pipe
  | `Pipe_bigstring pipe -> Pipe.close_read pipe
  | _ -> ()
;;

let is_pipe = function
  | `Pipe_string _ -> true
  | `Pipe_bigstring _ -> true
  | _ -> false
;;
