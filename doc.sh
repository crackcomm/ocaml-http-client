#!/usr/bin/env bash

esy dune build @doc
cd "$(esy echo '#{self.target_dir}')/default/_doc/_html/"
rm -rf .git
git init
git checkout -b gh-pages
git remote add github git@github.com:crackcomm/ocaml-http-client.git
git add -A
git commit -m "doc(*): odoc for $(cd ../; git rev-parse --short --quiet --verify HEAD)"
git push --force github gh-pages
