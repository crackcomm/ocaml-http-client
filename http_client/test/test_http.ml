(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Core
open Async

let ( let* ) x f = Deferred.bind ~f x
let ( let+ ) x f = Deferred.map ~f x

let printf f = Log.Global.printf ~level:`Debug f

module Server = struct
  open Httpaf

  let default_response_headers =
    Http_client.Headers.of_list
      [ "content-type", "application/json"; "X-UNUSUAL-KEY", "response-value-3123" ]
  ;;

  let test_handler ?(close_ = false) reqd resp b =
    let headers =
      String.length b
      |> Int.to_string
      |> Http_client.Headers.add default_response_headers "content-length"
    in
    let headers = if close_ then Http_client.Headers.add headers "connection" "close" else headers in
    Reqd.respond_with_string reqd (Response.create ~headers resp) b
  ;;

  let time_source = Time_source.wall_clock ()
  let pause_ = Time_ns.Span.of_sec 0.3

  let timeout_handler ?(close_ = false) reqd =
    Time_source.after time_source pause_ >>| fun _ -> test_handler ~close_ reqd `OK "{}"
  ;;

  let echo_handler reqd headers =
    let response =
      let content_type =
        match Http_client.Headers.get headers "content-type" with
        | None -> "application/octet-stream"
        | Some x -> x
      in
      let hdr =
        Http_client.Headers.of_list
          [ "content-type", content_type; "X-UNUSUAL-KEY", "response-value-3123" ]
      in
      let hdr =
        match Http_client.Headers.get headers "content-length" with
        | None -> Http_client.Headers.add hdr "connection" "close"
        | Some x -> Http_client.Headers.add hdr "content-length" x
      in
      Response.create ~headers:hdr `OK
    in
    let request_body = Reqd.request_body reqd in
    let response_body = Reqd.respond_with_streaming reqd response in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_bigstring response_body buffer ~off ~len;
      Body.Reader.schedule_read request_body ~on_eof ~on_read
    and on_eof () = Body.Writer.close response_body in
    Body.Reader.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  ;;

  let request_handler (_ : Socket.Address.Inet.t) reqd =
    match Reqd.request reqd with
    | { Request.meth = `GET; Request.target = "/empty"; _ } -> test_handler reqd `OK ""
    | { Request.meth = `GET; Request.target = "/test"; _ } -> test_handler reqd `OK "{}"
    | { Request.meth = `GET; Request.target = "/close"; _ } ->
      test_handler ~close_:true reqd `OK "{}"
    | { Request.meth = `GET; Request.target = "/timeout_close"; _ } ->
      don't_wait_for (timeout_handler ~close_:true reqd)
    | { Request.meth = `GET; Request.target = "/timeout"; _ } ->
      don't_wait_for (timeout_handler reqd)
    | { Request.meth = `POST; Request.target = "/echo"; headers; _ } ->
      echo_handler reqd headers
    | _ -> test_handler reqd `Method_not_allowed ""
  ;;

  let error_handler (_ : Socket.Address.Inet.t) ?request:_ error start_response =
    let response_body = start_response Http_client.Headers.empty in
    (match error with
    | `Exn exn ->
      Body.Writer.write_string response_body (Exn.to_string exn);
      Body.Writer.write_string response_body "\n"
    | #Status.standard as error ->
      Body.Writer.write_string response_body (Status.default_reason_phrase error));
    Body.Writer.close response_body
  ;;

  let create ?(max_connections = 1000) port =
    Tcp.(
      Server.create_sock
        ~on_handler_error:`Raise
        ~backlog:1000
        ~max_connections
        ~max_accepts_per_batch:1000
        (Where_to_listen.of_port port))
      (Httpaf_async.Server.create_connection_handler ~request_handler ~error_handler)
  ;;
end

let create_request ?(header = Httpaf.Headers.empty) ~host ~path method_ =
  let headers = Http_client.Headers.with_host header host in
  Httpaf.Request.create ~headers method_ path
;;

let request ?(method_ = `GET) ?body ?header ~host ~path conn =
  printf "Starting a request to %s" path;
  let timeout = Time_ns.Span.of_ms 50. in
  Http_client.connected conn
  >>= fun { r; w; _ } ->
  let req = create_request ?header ~host ~path method_ in
  Http_client.request ?body ~timeout req r w
  >>= fun resp -> Log.Global.flushed () >>| fun _ -> resp
;;

let print_response = function
  | Ok (resp : Http_client.Response.t) ->
    print_s [%sexp (Httpaf.Status.to_code resp.status : int)];
    print_s [%sexp (Bigstring.to_string resp.body : string)];
    print_s [%sexp (Bigstring.length resp.body : int)];
    print_s [%sexp (Httpaf.Headers.to_rev_list resp.headers : (string * string) list)]
  | Error e -> print_s [%sexp (e : Http_client.Error.t)]
;;

let reconnect_delay () = Time_ns.Span.of_sec 0.1

let%expect_test "main_test" =
  Server.create 2343
  >>= fun _server ->
  printf "server started";
  let host, port = "127.0.0.1", 2343 in
  let server_url = Uri.make ~scheme:"http" ~host ~port () in
  let conn = Http_client.create ~reconnect_delay ~server_name:"bnc_perp" server_url in
  let request ?method_ ?body ?header path =
    request ?method_ ?body ?header ~host ~path conn
  in
  let* res = request "/test" in
  print_response res;
  [%expect
    {|
    200
    {}
    2
    ((content-length 2) (X-UNUSUAL-KEY response-value-3123)
     (content-type application/json)) |}];
  let* res = request "/first" in
  print_response res;
  [%expect
    {|
    405
    ""
    0
    ((content-length 0) (X-UNUSUAL-KEY response-value-3123)
     (content-type application/json)) |}];
  let* res = request "/timeout" in
  print_response res;
  [%expect {| Timeout |}];
  let* res = request "/close" in
  print_response res;
  [%expect
    {|
    200
    {}
    2
    ((connection close) (content-length 2) (X-UNUSUAL-KEY response-value-3123)
     (content-type application/json)) |}];
  let* res = request "/empty" in
  print_response res;
  [%expect
    {|
    200
    ""
    0
    ((content-length 0) (X-UNUSUAL-KEY response-value-3123)
     (content-type application/json)) |}];
  let* res = request "/timeout_close" in
  print_response res;
  [%expect {| Timeout |}];
  let body = "example test body" in
  let size = String.length body in
  let header =
    Http_client.Headers.add Server.default_response_headers "content-length" (Int.to_string size)
  in
  let+ res = request ~method_:`POST ~body ~header "/echo" in
  print_response res;
  [%expect
    {|
    200
    "example test body"
    17
    ((content-length 17) (X-UNUSUAL-KEY response-value-3123)
     (content-type application/json)) |}]
;;

let%expect_test "main_f" =
  Server.create ~max_connections:2 2344
  >>= fun server ->
  printf "server started";
  let host, port = "127.0.0.1", 2344 in
  let server_url = Uri.make ~scheme:"http" ~host ~port () in
  let conn = Http_client.create ~reconnect_delay ~server_name:"bnc_perp" server_url in
  Http_client.connected conn
  >>= fun { r; w; _ } ->
  let req = create_request ~host ~path:"/timeout" `GET in
  let req = Http_client.request req r w in
  Tcp.Server.close ~close_existing_connections:true server
  >>= fun _ ->
  req
  >>| function
  | Error `Connection_failure -> printf "Connection_failure"
  | _ -> failwith "expected Malformed_response"
;;
