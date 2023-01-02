(lang dune 3.2)

(name http_client)

(generate_opam_files true)

(formatting
 (enabled_for ocaml))

(source
 (github crackcomm/ocaml-http-client))

(authors "\197\129ukasz Kurowski")

(maintainers "crackcomm@gmail.com")

(license LICENSE)

(documentation https://crackcomm.github.io/ocaml-http-client/http_client/index.html)

(package
 (name http_client)
 (synopsis "Rate limiting HTTP API functors.")
 (description "Rate limiting HTTP API functors.")
 (depends
  (odoc :with-doc)
  (atdgen :dev)
  (atdgen-runtime :dev)
  (async
   (>= 0.15.0))
  (core
   (>= 0.15.1))
  (uri
   (>= 4.2.0))
  (async-uri
   (> 0.3.0))
  (cohttp
   (>= 5.0.0))
  (cohttp-async
   (>= 5.0.0))
  (ppx_log
   (>= 0.15.0))
  (ppx_sexp_conv
   (>= 0.15.0))
  (ppx_deriving
   (>= 5.1))
  (httpaf-async
   (>= 0.7.1))
  (ocaml
   (>= 4.07.0))))
