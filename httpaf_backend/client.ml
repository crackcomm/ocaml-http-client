(* SPDX-License-Identifier: BSD-3-Clause *)

module Conn = struct
  include Async_uri
  include Conn

  let connect uri = Async_uri.connect uri
end

module Persistent = struct
  type conn = Conn.t

  include Async_uri.Persistent
end

let log_name = "Httpaf"
let call = Conn.call
