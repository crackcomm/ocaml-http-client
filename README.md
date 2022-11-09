# ocaml-http-client

HTTP API functors in OCaml with [http/af](https://github.com/inhabitedtype/httpaf) backend.

To know more about rate limiter see [Limiter_async](https://ocaml.org/p/async_kernel/v0.15.0/doc/Limiter_async/index.html) and [`enqueue_timeout'`](https://github.com/crackcomm/ocaml-http-client/blob/main/http_client/pool.ml#L99) implementation.

See example use of [`Route.Make`](https://github.com/crackcomm/ocaml-http-client/blob/main/http_api/make_route.ml#L9) ([doc](https://crackcomm.github.io/ocaml-http-client/http_client/Http_api/Route/Make/index.html)) in [examples](https://github.com/crackcomm/ocaml-http-client/blob/main/examples/public_api.ml#L17) directory.

## Copyright

[The 3-Clause BSD License](https://opensource.org/licenses/BSD-3-Clause)

