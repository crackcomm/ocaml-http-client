(* SPDX-License-Identifier: GPL-3.0-or-later *)

module Exchange_info :
  Http_api.Route.S with type Request.t := unit and type Response.t := Api_t.exchange_info
