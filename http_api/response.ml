(* SPDX-License-Identifier: BSD-3-Clause *)

open Async
open Core
open Http_types
include Response_intf

module Of_string = struct
  module Make_on_success (R : Of_string_with_error) :
    S with type t = R.t and module Error = R.Error = struct
    include R

    let parse_response status body =
      Body.to_string' body
      >>| fun resp ->
      if Status_code.is_success status
      then Ok (R.of_string resp)
      else Error (R.Error.of_string ~status resp)
    ;;
  end

  module Make_self_error (R : Of_string_with_status) :
    S with type t = R.t and module Error = R = struct
    include R
    module Error = R

    let parse_response status body =
      Body.to_string' body
      >>| fun resp ->
      if Status_code.is_success status
      then Ok (R.of_string ~status resp)
      else Error (R.of_string ~status resp)
    ;;
  end
end
