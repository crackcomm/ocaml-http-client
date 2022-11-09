(* SPDX-License-Identifier: BSD-3-Clause *)

module Status : sig
  include module type of struct
    include Httpaf.Status
  end
end

module Method : sig
  include module type of struct
    include Httpaf.Method
  end
end

module Headers : sig
  include module type of struct
    include Httpaf.Headers
  end

  (** [with_host headers url] Appends `Host` header. *)
  val with_host : t -> string -> t

  (** [with_host_of_uri headers url] Appends `Host` header for [url]. *)
  val with_host_of_uri : t -> Uri.t -> t

  (** [reverse headers] Reverse headers order. *)
  val reverse : t -> t
end

module Query : sig
  module Key : sig
    (** Query parameter key. *)
    type t = string
  end

  module Value : sig
    (** Query parameter value. *)
    type t = string
  end

  module Param : sig
    (** Query parameter values identified by a key. *)
    type t = Key.t * Value.t list

    (** [of_kv key value] Creates a query parameter with single value. *)
    val of_kv : Key.t -> Value.t -> t
  end

  (** Query type. *)
  type t = Param.t list

  (** Empty query parameters.*)
  val empty : t

  (** [with_param query key value] Creates a parameter and appends to query. *)
  val with_param : t -> Key.t -> Value.t -> t

  (** [sort query] Short for [List.sort]. *)
  val sort : t -> t
end

module Retry_delay : sig
  val rand_delay
    :  ?retry_delay:(unit -> Core.Time_ns.Span.t)
    -> ?random_state:Core.Random.State.t
    -> ?time_source:Async.Time_source.t
    -> unit
    -> unit Async.Deferred.t
end
