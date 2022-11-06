(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Core
open Async
open Common

module Make_response_on_success (R : sig
  type t

  val of_string : string -> t

  module Error_message : sig
    val of_string : string -> Error_message.t
  end
end) : Make_route_intf.S_response with type t = R.t = struct
  type t = R.t

  let read_response s body =
    if Httpaf.Status.is_successful s
    then Ok (R.of_string body)
    else (
      let msg = R.Error_message.of_string body in
      Error msg)
  ;;
end

module Make_success_on_codes (R : sig
  type t

  val of_string : string -> t
  val code_of : t -> int
  val ok_codes : int list

  module Error_message : sig
    val of_response : t -> Error_message.t
  end
end) : Make_route_intf.S_response with type t = R.t = struct
  type t = R.t

  let read_response s body =
    let res = R.of_string body in
    if Httpaf.Status.is_successful s
       || List.mem ~equal:Int.equal R.ok_codes (R.code_of res)
    then Ok res
    else (
      let msg = R.Error_message.of_response res in
      Error msg)
  ;;
end

module Request_id = struct
  module T = struct
    type t =
      | Started of Int.t * Time_ns_unix.t
      | Finished of Int.t * Time_ns_unix.t * Time_ns_unix.Span.t
    [@@deriving bin_io, compare, hash, sexp]

    include Sexpable.To_stringable (struct
      type nonrec t = t [@@deriving sexp]
    end)

    let module_name = "Route.Request_id"
  end

  include T
  include Identifiable.Make (T)

  let create () =
    let next_req_id = ref 0 in
    fun () ->
      let id = !next_req_id in
      next_req_id := id + 1;
      Started (id, Time_ns_unix.now ())
  ;;

  let finish_ = function
    | Started (id, created) ->
      let span = Time_ns_unix.diff (Time_ns_unix.now ()) created in
      Finished (id, created, span)
    | _ -> failwith "called finish_ on finished"
  ;;
end

let outcome_of_response
    (type resp)
    ~req_id
    (module E : Make_route_intf.Descriptor with type Response.t = resp)
    resp
  =
  let req_id = Request_id.finish_ req_id in
  match resp with
  | Http_client.Pool.Outcome.Response resp ->
    let read_response (resp : Http_client.Response.t) =
      Bigstring.to_string resp.body |> E.Response.read_response resp.status
    in
    (match read_response resp with
    | Ok r ->
      [%log.global.debug
        "success response" (E.method_name : string) (req_id : Request_id.t)];
      Outcome.Success r
    | Error e ->
      [%log.global.error
        "error response"
          (E.method_name : string)
          (req_id : Request_id.t)
          (e.code : int)
          (e.message : string)];
      Error_message e)
  | Timeout ->
    [%log.global.debug "request Timeout" (E.method_name : string) (req_id : Request_id.t)];
    Outcome.Timeout
  | Queue_timeout ->
    [%log.global.debug
      "request Queue_timeout" (E.method_name : string) (req_id : Request_id.t)];
    Outcome.Queue_timeout
  | Connection_failure ->
    [%log.global.debug
      "request Connection_failure" (E.method_name : string) (req_id : Request_id.t)];
    Outcome.Connection_failure
  | Aborted -> failwith "connection pool was killed"
  | Raised e -> failwith ("client exception: " ^ Exn.to_string e)
;;

let fetch_exn req =
  let rec fetch () =
    req ()
    >>= function
    | Outcome.Success s -> Deferred.return s
    | Connection_failure | Timeout | Queue_timeout -> Http_client.Retry_delay.rand_delay () >>= fetch
    | Error_message e -> failwith (sprintf "error code %d message: %s" e.code e.message)
  in
  fetch ()
;;
