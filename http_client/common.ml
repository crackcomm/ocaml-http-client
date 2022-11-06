(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Core

module Status = Httpaf.Status
module Method = Httpaf.Method

module Headers = struct
  include Httpaf.Headers

  let with_host headers host = add headers "Host" host

  let with_host_of_uri headers url =
    match Uri.host url, Uri.port url with
    | Some h, Some p -> with_host headers (h ^ ":" ^ Int.to_string p)
    | Some h, None -> with_host headers h
    | _ -> headers
  ;;

  let reverse headers = to_rev_list headers |> List.rev |> of_rev_list
end

module Query = struct
  module Key = struct
    type t = string
  end

  module Value = struct
    type t = string
  end

  module Param = struct
    type t = Key.t * Value.t list

    let of_kv key value = key, [ value ]
    let compare_key (x, _) (y, _) = String.compare x y
  end

  type t = Param.t list

  let empty = []
  let with_param q k v = q @ [ Param.of_kv k v ]
  let sort = List.sort ~compare:Param.compare_key
end

module Retry_delay = struct
  open Core
  open Async

  let default_delay () =
    if am_running_test then Time_ns.Span.of_sec 0.1 else Time_ns.Span.of_sec 10.
  ;;

  let rand_delay
      ?(retry_delay = default_delay)
      ?(random_state = Random.State.default)
      ?(time_source = Time_source.wall_clock ())
      ()
    =
    let span = Time_ns.Span.to_sec (retry_delay ()) in
    let distance = Random.State.float random_state (span *. 0.3) in
    let wait =
      if Random.State.bool random_state then span +. distance else span -. distance
    in
    Time_source.after time_source (Time_ns.Span.of_sec wait)
  ;;
end
