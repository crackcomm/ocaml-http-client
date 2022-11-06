(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Core
open Async
open Async_uri
open Common
include Async_uri.Persistent

let on_event ev =
  [%log.global.debug "connection event" (ev : Uri_sexp.t Persistent.Event.t)];
  Deferred.unit
;;

let create ~server_name ?(on_event = on_event) ?reconnect_delay:retry_delay url =
  let connect addr = Deferred.Or_error.try_with (fun _ -> Async_uri.connect addr) in
  let address () = Deferred.Or_error.return url in
  Persistent.create
    ~on_event
    ?retry_delay
    ~connect
    ~server_name
    ~address:(module Uri_sexp)
    address
;;

module Request = struct
  let create ?(headers = Headers.empty) ?query ~method_ ~host ~path () =
    let headers = Headers.with_host headers host in
    let path =
      match query with
      | Some q -> Format.sprintf "%s?%s" path (Uri.encoded_of_query q)
      | None -> path
    in
    Httpaf.Request.create ~headers method_ path
  ;;
end

module Response = struct
  type t =
    { status : Httpaf.Status.t
    ; headers : Httpaf.Headers.t
    ; body : Bigstring.t
    }

  type internal_ =
    { status : Httpaf.Status.t
    ; headers : Httpaf.Headers.t
    ; body : Bigstring.t
    ; is_persistent : bool
    }
end

module Error = struct
  type t =
    [ `Raised of exn
    | `Timeout
    | `Connection_failure
    ]
  [@@deriving sexp_of]
end

let response_handler ~request_method process_var r body =
  match Httpaf.Response.body_length ~request_method r with
  | `Fixed size ->
    let buf = Bigstring.create (Int.of_int64_exn size) in
    let rec loop index =
      Httpaf.Body.Reader.schedule_read
        body
        ~on_eof:(fun () ->
          Ok
            Response.
              { body = buf
              ; status = r.status
              ; headers = r.headers
              ; is_persistent = Httpaf.Response.persistent_connection r
              }
          |> Ivar.fill process_var)
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
        ~on_eof:(fun () ->
          let buf = Bigstring.concat (List.rev chunks) in
          Ok
            Response.
              { body = buf
              ; status = r.status
              ; headers = r.headers
              ; is_persistent = Httpaf.Response.persistent_connection r
              }
          |> Ivar.fill process_var)
        ~on_read:(fun x ~off ~len ->
          let buf = Bigstring.sub ~pos:off ~len x in
          loop (buf :: chunks))
    in
    loop []
;;

let method_std : Httpaf.Method.t -> Httpaf.Method.standard = function
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

(* Default buffer size is 32kB. *)
let buffer_size = 32 * 1024

let request ?timeout ?body req r w =
  let process_var = Ivar.create () in
  (* [%log.global.debug
    "sending request" (req.target : string) (Method.to_string req.meth : string)]; *)
  let error_handler e =
    let err =
      match e with
      | `Exn e -> `Raised e
      | `Invalid_response_body_length _ -> `Connection_failure
      | `Malformed_response _ -> `Connection_failure
    in
    [%log.global.debug "closing reader and writer" (err : Error.t)];
    don't_wait_for (Writer.close w);
    don't_wait_for (Reader.close r);
    Ivar.fill_if_empty process_var @@ Error err
  in
  let request_method = Httpaf.Request.(method_std req.meth) in
  let response_handler = response_handler ~request_method process_var in
  let bw, conn = Httpaf.Client_connection.request req ~error_handler ~response_handler in
  let read_complete = Ivar.create () in
  let buffer = Httpaf_async.Buffer.create buffer_size in
  let rec reader_thread () =
    match Httpaf.Client_connection.next_read_operation conn with
    | `Close ->
      Ivar.fill_if_empty read_complete ();
      Deferred.unit
    | `Read ->
      Reader.read_one_chunk_at_a_time
        r
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
      | `Stopped () -> reader_thread ())
  in
  let write_complete = Ivar.create () in
  let rec writer_thread () =
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
      writer_thread ()
    | `Yield -> Httpaf.Client_connection.yield_writer conn writer_thread
    | `Close _ -> Ivar.fill_if_empty write_complete ()
  in
  let monitor = Monitor.create () in
  don't_wait_for (Scheduler.within' ~monitor reader_thread);
  Scheduler.within ~monitor writer_thread;
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
  Deferred.any [ Ivar.read process_var; timeout ]
  >>= fun res ->
  match res with
  | Ok { is_persistent; status; body; headers } ->
    let resp = Ok Response.{ status; body; headers } in
    if not is_persistent
    then Writer.close w >>= fun _ -> Reader.close r >>| fun _ -> resp
    else
      Deferred.all_unit [ Ivar.read read_complete; Ivar.read write_complete ]
      >>| fun _ -> resp
  | Error `Timeout ->
    Writer.close w >>= fun _ -> Reader.close r >>| fun _ -> Error `Timeout
  | Error e -> Deferred.return (Error e)
;;
