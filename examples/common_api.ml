(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Http_api

module Error_message = struct
  type t = Api_t.response_message

  let of_response (res : t) =
    { Route.Error_message.code = res.code; message = res.message }
  ;;

  let of_string s = of_response (Api_j.response_message_of_string s)
end


module Make_success_on_codes (S : sig
  val ok_codes : int list
end) =
Route.Make_success_on_codes (struct
  type t = Api_t.response_message

  let ok_codes = S.ok_codes
  let code_of (res : t) = res.code
  let of_string = Api_j.response_message_of_string

  module Error_message = Error_message
end)
