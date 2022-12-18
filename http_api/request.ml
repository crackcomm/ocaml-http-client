(* SPDX-License-Identifier: BSD-3-Clause *)

open Http_types

module type S_make = sig
  type t [@@deriving sexp_of]

  val path : t -> string
  val meth : t -> Method.t
  val query : t -> Query.t option
  val headers : t -> Headers.t
  val body : t -> Body.t

  (** Weight for rate limiter. *)
  val weight : t -> int
end

module type S = sig
  type t [@@deriving sexp_of]

  val uri : t -> Uri.t
  val meth : t -> Method.t
  val headers : t -> Headers.t
  val request : t -> Request.t

  (** Weight for rate limiter. *)
  val weight : t -> int
end

module Make (R : S_make) : S with type t = R.t = struct
  include R

  let uri r = Uri.make ~scheme:"https" ?query:(R.query r) ~path:(R.path r) ()
  let request r = Request.make ~body:(R.body r) ~headers:(R.headers r) (R.meth r) (uri r)
end

module Make_body_json (R : sig
  type t [@@deriving sexp_of]

  val path : t -> string
  val meth : t -> Method.t
  val weight : t -> int
  val query : t -> Query.t option
  val headers : t -> Headers.t
  val body : t -> string
end) : S with type t = R.t = Make (struct
  include R

  let body r = Body.of_string (R.body r)
end)

module Make_no_body (R : sig
  type t [@@deriving sexp_of]

  val path : t -> string
  val meth : t -> Method.t
  val weight : t -> int
  val query : t -> Query.t option
  val headers : t -> Headers.t
end) : S with type t = R.t = Make (struct
  include R

  let body _ = `Empty
end)

module Make_no_headers (R : sig
  type t [@@deriving sexp_of]

  val path : t -> string
  val meth : t -> Method.t
  val weight : t -> int
  val query : t -> Query.t option
end) : S with type t = R.t = Make (struct
  include R

  let body _ = `Empty
  let headers _ = []
end)

module Make_get (R : sig
  type t [@@deriving sexp_of]

  val path : t -> string
  val weight : t -> int
  val query : t -> Query.t option
end) : S with type t = R.t = Make_no_headers (struct
  include R

  let meth _ = `GET
end)
