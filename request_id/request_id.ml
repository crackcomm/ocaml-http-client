(* SPDX-License-Identifier: BSD-3-Clause *)

open Core

module T = struct
  type t =
    | Started of Int.t * Time_ns.Span.t
    | Finished of Int.t * Time_ns.Span.t * Time_ns.Span.t
  [@@deriving bin_io, compare, hash, sexp]

  include Sexpable.To_stringable (struct
    type nonrec t = t [@@deriving sexp]
  end)

  let module_name = "Route.Request_id"
end

include T
include Identifiable.Make (T)

let now () = Time_ns.(now () |> to_span_since_epoch)
let value_or_now = Option.value_or_thunk ~default:now

let create () =
  let next_req_id = ref 0 in
  fun ?now () ->
    let id = !next_req_id in
    next_req_id := id + 1;
    Started (id, value_or_now now)
;;

let finish_ ?now = function
  | Started (id, created) ->
    let finished = value_or_now now in
    Finished (id, created, finished)
  | _ -> failwith "called finish_ on finished"
;;

let id = function
  | Started (id, _) -> id
  | Finished (id, _, _) -> id
;;

let span = function
  | Started _ -> None
  | Finished (_, created, finished) -> Some Time_ns.Span.(finished - created)
;;

let span_exn = function
  | Started _ -> failwith "called [span_exn] on unfinished [Req_id]"
  | Finished (_, created, finished) -> Time_ns.Span.(finished - created)
;;

let%test_unit "create generates sequential IDs" =
  let now = now () in
  let create = create () in
  let r1 = create ~now () in
  let r2 = create ~now () in
  let r3 = create ~now () in
  [%test_result: t] ~expect:(Started (0, now)) r1;
  [%test_result: t] ~expect:(Started (1, now)) r2;
  [%test_result: t] ~expect:(Started (2, now)) r3
;;

let%test_unit "finish_ fails on finished request" =
  let r1 = create () () in
  let r2 = finish_ r1 in
  assert (does_raise (fun () -> finish_ r2))
;;

let%test_unit "create generates unique IDs" =
  let create = create () in
  let len = 10000 in
  let request_ids = List.init len ~f:(fun _ -> create ()) in
  let unique_ids = List.map request_ids ~f:(fun request_id -> id request_id) in
  let unique_set = Int.Set.of_list unique_ids in
  [%test_result: Int.t] ~expect:len (Int.Set.length unique_set)
;;

let%test_unit "finish_ finishes started request and returns span" =
  let start = now () in
  let request_id = create () ~now:start () in
  let req_span = Time_ns.Span.of_min 1. in
  let finish = Time_ns.Span.(start + req_span) in
  let finished_request_id = finish_ ~now:finish request_id in
  [%test_result: Time_ns.Span.t option] ~expect:None (span request_id);
  [%test_result: Time_ns.Span.t option] ~expect:(Some req_span) (span finished_request_id)
;;
