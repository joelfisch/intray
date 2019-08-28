#!/bin/bash

set -e
set -x

stack install :intray-web-server --file-watch --exec='./scripts/restart-intray-web-server.sh' --fast --ghc-options=-freverse-errors $@
