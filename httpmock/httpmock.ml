(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Http_types

module Make (M : sig
  val response : Response.t
end) : Http_backend_intf.Client.S = struct
  open M

  module Conn : Http_backend_intf.Client.S_conn = struct
    type t = unit

    let connect _ = Deferred.unit
  end

  (* log_name of the backend for logs *)
  let log_name = "mock"

  (*
   *   Trying 192.168.190.30:8080...
   * TCP_NODELAY set
   * Connected to one.ocxmr (192.168.190.30) port 8080 (#0)
   > GET /dashboard/self HTTP/1.1
   > Host: one.ocxmr:8080
   > User-Agent: curl/7.68.0
   > Accept: */*
   >
   * Mark bundle as not supporting multiuse
   < HTTP/1.1 200 OK
   < Date: Wed, 14 Dec 2022 12:16:54 GMT
   < X-Frame-Options: DENY
   < Set-Cookie: XSRF_TOKEN=; Path=/; Expires=Thu, 01-Jan-1970 00:00:00 GMT; Max-Age=0
   < Expires: Thu, 01 Jan 1970 00:00:00 GMT
   < Content-Type: text/html;charset=utf-8
   < Content-Length: 4913
   <
   <!DOCTYPE html>
   *)
  let call ?timeout:_ _ (req : Request.t) =
    printf
      "> %s %s HTTP/1.1\n"
      (Cohttp.Code.string_of_method (req.meth :> Cohttp.Code.meth))
      (Uri.path_and_query req.uri);
    List.iter req.headers ~f:(fun (k, v) -> printf "> %s: %s\n" k v);
    print_string ">\n";
    (let finish_ () =
       print_endline "";
       Deferred.unit
     in
     match req.body with
     | `Empty -> Deferred.unit
     | `String s -> print_endline s |> return
     | `Strings s -> List.iter s ~f:print_string |> finish_
     | `Pipe s -> Pipe.iter_without_pushback s ~f:print_string >>= finish_)
    >>| fun _ -> Ok response
  ;;
end

let print_request' ?(uri = Uri.empty) request =
  let module T =
    Make (struct
      let response = Response.{ status = 200; body = `Empty; headers = [] }
    end)
  in
  let client = Http_client.create_test (module T) uri in
  Http_client.call client request >>| ignore
;;

let print_request ?uri request =
  Thread_safe.block_on_async_exn (fun () -> print_request' ?uri request)
;;
