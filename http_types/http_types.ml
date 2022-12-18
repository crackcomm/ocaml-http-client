(* SPDX-License-Identifier: BSD-3-Clause *)

(* This library is generic to backends. It relies on `Cohttp` for functions such as `Code.is_success` etc.

 Body module was inspired by `Cohttp` but extended to support `Bigstring.t`. *)

include Common
module Body = Body
module Request = Request
module Response = Response
