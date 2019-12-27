#!/bin/bash

set -e
set -x

stack install :intray-web-server \
  --file-watch \
  --fast \
  --ghc-options=-freverse-errors \
  --exec='./scripts/restart-intray-web-server-free.sh' \
  $@
