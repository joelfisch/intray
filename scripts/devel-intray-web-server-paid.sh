#!/usr/bin/env bash

set -e
set -x

function check_set () {
  if [ -z ${!1+x} ]; then
    echo "$1 must be set"
    return 1
  fi
}
check_set INTRAY_SERVER_STRIPE_PLAN
check_set INTRAY_SERVER_STRIPE_SECRET_KEY
check_set INTRAY_SERVER_STRIPE_PUBLISHABLE_KEY

stack install :intray-web-server \
  --file-watch \
  --fast \
  --ghc-options=-freverse-errors \
  --exec='./scripts/restart-intray-web-server-paid.sh' \
  $@
