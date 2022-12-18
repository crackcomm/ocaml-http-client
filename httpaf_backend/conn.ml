(* SPDX-License-Identifier: BSD-3-Clause *)

open Core
open Async
open Http_types

let method_std : Method.t -> Httpaf.Method.standard = function
  | `GET -> `GET
  | `HEAD -> `HEAD
  | `POST -> `POST
  | `PUT -> `PUT
  | `DELETE -> `DELETE
  | `CONNECT -> `CONNECT
  | `OPTIONS -> `OPTIONS
  | `TRACE -> `TRACE
  (* Assume method behaves like POST. *)
  | `Other _ -> `POST
;;

(** Creates Httpaf.Body.Reader.t for a response depending on known body length. *)
let create_bigstring_list_reader
    ~request_method
    ~fill_bigstring
    ~fill_bigstring_list
    r
    body
  =
  match Httpaf.Response.body_length ~request_method r with
  | `Fixed size ->
    let buf = Bigstring.create (Int.of_int64_exn size) in
    let rec loop index =
      Httpaf.Body.Reader.schedule_read
        body
        ~on_eof:(fun () -> fill_bigstring buf)
        ~on_read:(fun x ~off ~len ->
          Bigstring.blit ~src:x ~dst:buf ~dst_pos:index ~src_pos:off ~len;
          loop (index + len))
    in
    loop 0
  | `Close_delimited -> failwith "body_length Close_delimited unimplemented"
  | `Error _ -> ()
  | `Chunked ->
    let rec loop chunks =
      Httpaf.Body.Reader.schedule_read
        body
        ~on_eof:(fun () -> fill_bigstring_list (List.rev chunks))
        ~on_read:(fun x ~off ~len ->
          let buf = Bigstring.sub ~pos:off ~len x in
          loop (buf :: chunks))
    in
    loop []
;;

let create_bigstring_pipe_reader ~to_body w body =
  let rec loop () =
    Httpaf.Body.Reader.schedule_read
      body
      ~on_eof:(fun () -> ()) (*Pipe.close w)*)
      ~on_read:(fun x ~off ~len ->
        let buf = Bigstring.sub ~pos:off ~len x in
        Pipe.write_without_pushback w (to_body buf);
        loop ())
  in
  loop ()
;;

let create_ignore_reader fill_body body =
  let rec loop () =
    Httpaf.Body.Reader.schedule_read
      body
      ~on_eof:(fun () -> fill_body ())
      ~on_read:(fun _ ~off:_ ~len:_ -> loop ())
  in
  loop ()
;;

(** Construct a response handler taking into consideration the user suggestion on [how] he
    wants to receive the response. Right now we handle cases for [`Ignore],
    [`List `Bigstring] and [`Pipe `Bigstring].

    If the response is of [`Fixed size] type, the response body will be a single
    [Bigstring.t]. If the response is [`Chunked], the body can be piped or returned as a
    list or concatenated into a single [Bigstring.t]. *)
let create_response_handler
    ~(how : Body.How.t)
    ~request_method
    (response_var : (Httpaf.Response.t * Body.t, 'a) Result.t Ivar.t)
    response
  =
  let request_method = method_std request_method in
  let how =
    match how, Httpaf.Response.body_length ~request_method response with
    | _, `Close_delimited -> failwith "body_length Close_delimited unimplemented"
    | `Ignore, `Fixed _ -> `Ignore
    | _, `Fixed _ -> `Bigstring_concat
    | `Ignore, `Error _ -> `Ignore
    | _, `Error _ -> `Bigstring_concat
    | `Bigstrings, `Chunked -> `Bigstring_list
    | `Pipe_string, `Chunked -> `String_pipe
    | (`Pipe_bigstring | _), `Chunked -> `Bigstring_pipe
  in
  let fill_response_body body = Ivar.fill response_var (Ok (response, body)) in
  let response_handler =
    match how with
    | `Ignore -> create_ignore_reader (fun () -> fill_response_body `Empty)
    | `Bigstring_list ->
      create_bigstring_list_reader
        ~request_method
        ~fill_bigstring:(fun buf -> fill_response_body (Body.of_bigstring buf))
        ~fill_bigstring_list:(fun buf -> fill_response_body (Body.of_bigstring_list buf))
        response
    | `Bigstring_concat ->
      create_bigstring_list_reader
        ~request_method
        ~fill_bigstring:(fun buf -> fill_response_body (Body.of_bigstring buf))
        ~fill_bigstring_list:(fun buf ->
          fill_response_body (Body.of_bigstring (Bigstring.concat buf)))
        response
    | `Bigstring_pipe ->
      let r, w = Pipe.create () in
      Ivar.fill response_var (Ok (response, `Pipe_bigstring r));
      create_bigstring_pipe_reader ~to_body:Fn.id w
    | `String_pipe ->
      let r, w = Pipe.create () in
      Ivar.fill response_var (Ok (response, `Pipe_string r));
      create_bigstring_pipe_reader ~to_body:Bigstring.to_string w
  in
  response_handler
;;

(* Default buffer size is 32kB. *)
let buffer_size = 32 * 1024 * 1024

let reader_loop ~read_complete conn reader =
  let rec reader_loop () =
    let buffer = Httpaf_async.Buffer.create buffer_size in
    match Httpaf.Client_connection.next_read_operation conn with
    | `Close ->
      Ivar.fill_if_empty read_complete ();
      Deferred.unit
    | `Read ->
      Reader.read_one_chunk_at_a_time
        reader
        ~handle_chunk:(fun buf ~pos:src_pos ~len:src_len ->
          let (n : int) =
            Httpaf_async.Buffer.put buffer ~f:(fun dst ~off:dst_pos ~len ->
                let len = if len < src_len then len else src_len in
                Bigstring.blit ~src:buf ~src_pos ~dst ~dst_pos ~len;
                len)
          in
          let (_ : int) =
            Httpaf_async.Buffer.get buffer ~f:(fun src ~off ~len ->
                Httpaf.Client_connection.read conn src ~off ~len)
          in
          return @@ `Stop_consumed ((), n))
      >>= (function
      | `Eof ->
        (* Read remainder of data from connection buffer. *)
        let buf = Bigstring.create 0 in
        ignore @@ Httpaf.Client_connection.read_eof conn buf ~off:0 ~len:0;
        Deferred.unit
      | `Eof_with_unconsumed_data data ->
        (* Read remainder of data from connection buffer. *)
        let len = String.length data in
        let buf = Bigstring.of_string ~len data in
        ignore @@ Httpaf.Client_connection.read_eof conn buf ~off:0 ~len;
        Deferred.unit
      | `Stopped () -> reader_loop ())
  in
  reader_loop ()
;;

let writer_loop ~write_complete conn w =
  let rec writer_loop () =
    match Httpaf.Client_connection.next_write_operation conn with
    | `Write iovecs ->
      let write_iovecs writer iovecs =
        List.fold_left
          iovecs
          ~init:0
          ~f:(fun written ({ Httpaf.IOVec.len; _ } as iovec) ->
            Writer.schedule_iovec writer (Obj.magic iovec);
            written + len)
      in
      let result = `Ok (write_iovecs w iovecs) in
      Httpaf.Client_connection.report_write_result conn result;
      writer_loop ()
    | `Yield -> Httpaf.Client_connection.yield_writer conn writer_loop
    | `Close _ -> Ivar.fill_if_empty write_complete ()
  in
  writer_loop ()
;;

(* If user specified a [`Pipe] in [read] we can stream the results into the [pipe]. *)
let request
    ?timeout
    ?body
    ?(read : Body.How.t = `Pipe_bigstring)
    (req : Httpaf.Request.t)
    r
    w
  =
  let response_var = Ivar.create () in
  let error_handler e =
    let err =
      match e with
      | `Exn e ->
        [%log.global.debug "Raised" (e : exn)];
        `Raised e
      | `Invalid_response_body_length _ ->
        [%log.global.debug "Invalid_response_body_length"];
        `Connection_failure
      | `Malformed_response e ->
        [%log.global.debug "Malformed_response" (e : string)];
        `Connection_failure
    in
    [%log.global.debug "closing reader and writer" (err : Response.Error.t)];
    don't_wait_for (Writer.close w);
    don't_wait_for (Reader.close r);
    Ivar.fill_if_empty response_var @@ Error err
  in
  let request_method = Httpaf.Request.(req.meth) in
  let response_handler = create_response_handler ~how:read ~request_method response_var in
  let bw, conn = Httpaf.Client_connection.request req ~error_handler ~response_handler in
  let monitor = Monitor.create () in
  let read_complete, write_complete = Ivar.create (), Ivar.create () in
  (* Start reading loop in the background. *)
  don't_wait_for
    (Scheduler.within' ~monitor (fun () -> reader_loop ~read_complete conn r));
  (* TODO: Users can supply pipes and create streaming RPC because we return early. *)
  Scheduler.within ~monitor (fun () -> writer_loop ~write_complete conn w);
  let _ =
    match body with
    | None -> ()
    | Some body -> Httpaf.Body.Writer.write_string bw body
  in
  Httpaf.Body.Writer.close bw;
  Monitor.detach_and_iter_errors monitor ~f:(Httpaf.Client_connection.report_exn conn);
  let timeout =
    match timeout with
    | None -> Deferred.never ()
    | Some timeout -> Clock_ns.after timeout >>| fun () -> Error `Timeout
  in
  Deferred.any [ Ivar.read response_var; timeout ]
  >>= fun res ->
  match res with
  | Ok (response, body) ->
    return
      (Ok
         Response.
           { status = Httpaf.Status.to_code response.status
           ; headers = Httpaf.Headers.to_list response.headers
           ; body
           })
    (*
    TODO: test consecutive requests on both pooled and regular connection

    let resp = Ok Response_.{ status; body; headers } in
    if not is_persistent
    then Writer.close w >>= fun _ -> Reader.close r >>| fun _ -> resp
    else
      Deferred.all_unit [ Ivar.read read_complete; Ivar.read write_complete ]
      >>| fun _ -> resp
    *)
  | Error `Timeout ->
    Writer.close w >>= fun _ -> Reader.close r >>| fun _ -> Error `Timeout
  | Error e -> Deferred.return (Error e)
;;

let convert_request ~body_length (req : Request.t) =
  let headers =
    match body_length with
    | Some len -> Headers.with_content_length req.headers len
    | None -> req.headers
  in
  let headers = Httpaf.Headers.of_list headers in
  Httpaf.Request.create ~headers (req.meth :> Httpaf.Method.t) (Request.path req)
;;

let call ?timeout Async_uri.{ r; w; _ } (req : Request.t) : Response.Result.t Deferred.t =
  (* TODO: Handle different types of request body. *)
  let body_length = Body.length_opt req.body in
  Body.to_string_opt' req.body
  >>= fun body ->
  [%log.global.debug
    "Httpaf sending"
      (req.uri : Uri_sexp.t)
      (req.meth : Method.t)
      (req.body : Body.t)
      (req.headers : Headers.t)];
  request ?timeout ?body (convert_request ~body_length req) r w
;;
