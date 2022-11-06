(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Common
open Core
open Async

module type S_request = sig
  (** Request type. *)
  type t

  (** HTTP path of the route. *)
  val path : string

  (** HTTP method of the route. *)
  val method_ : Httpaf.Method.t

  (** Returns weight of the request. *)
  val weight : t Weight.t

  (** Returns query parameters of the request. *)
  val query : t -> Http_client.Query.t option
end

module type S_response = sig
  (** Deserialized response type. *)
  type t

  (** Parse response into [t]. *)
  val read_response : Httpaf.Status.t -> string -> (t, Error_message.t) Result.t
end

module type Descriptor = sig
  module Request : S_request
  module Response : S_response

  (** Human readable method name. *)
  val method_name : string
end

module type S = sig
  include Descriptor

  val request
    :  ?timeout:Time_ns.Span.t
    -> Http_client.Pool.t
    -> Request.t
    -> Response.t Outcome.t Deferred.t

  (** Fetch in infinite loop. Used only in pathological cases. *)
  val fetch_exn : Http_client.Pool.t -> Request.t -> Response.t Deferred.t
end

module type Route = sig
  module type S_request = S_request
  module type S_response = S_response
  module type Descriptor = Descriptor
  module type S = S

  include module type of Common

  (* include module type of Route *)

  module Make (E : Descriptor) :
    S with type Request.t := E.Request.t and type Response.t := E.Response.t

  module Make_response_on_success (R : sig
    type t

    val of_string : string -> t

    module Error_message : sig
      val of_string : string -> Error_message.t
    end
  end) : sig
    type t

    val read_response : Httpaf.Status.t -> string -> (t, Error_message.t) Result.t
  end
  with type t = R.t

  module Make_success_on_codes (R : sig
    type t

    val of_string : string -> t
    val code_of : t -> int
    val ok_codes : int list

    module Error_message : sig
      val of_response : t -> Error_message.t
    end
  end) : S_response with type t = R.t

  module Request_id : sig
    include Identifiable

    (** [create ()] Creates a request id counter. *)
    val create : unit -> unit -> t

    (** [finish_ t] Returns request id that contains span of request. *)
    val finish_ : t -> t
  end

  val outcome_of_response
    :  req_id:Request_id.t
    -> (module Descriptor with type Response.t = 'resp)
    -> Http_client.Response.t Http_client.Pool.Outcome.t
    -> 'resp Outcome.t

  val fetch_exn : (unit -> 'a Outcome.t Deferred.t) -> 'a Deferred.t
end
