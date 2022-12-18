(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Http_types
open Httpmock

let%expect_test "create: uses host from uri with empty conn uri" =
  let uri = Uri.make ~path:"/test" ~host:"ocxmr.io" ~query:[ "a", [ "b"; "c" ] ] () in
  print_request (Request.make `GET uri);
  [%expect {|
    > GET /test?a=b,c HTTP/1.1
    > Host: ocxmr.io
    >
  |}]
;;

let%expect_test "create: uses host from conn if set" =
  let uri = Uri.make ~path:"/test" ~host:"ocxmr.io" ~query:[ "a", [ "b"; "c" ] ] () in
  print_request ~uri:(Uri.make ~host:"w1.ocxmr.io" ()) (Request.make `GET uri);
  [%expect {|
    > GET /test?a=b,c HTTP/1.1
    > Host: w1.ocxmr.io
    >
  |}]
;;

let%expect_test "mock body" =
  let body = `Pipe_string (Pipe.of_list [ "hello"; "world" ]) in
  let uri = Uri.make ~path:"/test" ~host:"ocxmr.io" ~query:[ "a", [ "b"; "c" ] ] () in
  print_request (Request.make ~body `GET uri);
  [%expect
    {|
    > GET /test?a=b,c HTTP/1.1
    > Host: ocxmr.io
    >
    helloworld
  |}]
;;
