#!/usr/bin/env bash
set -euo pipefail

nix fmt -- --fail-on-change --no-cache
sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --load cl-deflate-kit.asd \
  --eval '(asdf:operate (quote asdf:compile-op) "cl-deflate-kit" :force t)' \
  --eval '(quit)'
