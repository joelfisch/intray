#!/usr/bin/env bash
set -x

cd $HOME

killall -f 'intray-web-server serve' || true

set -e

export INTRAY_SERVER_LOG_LEVEL=LevelDebug

intray-web-server serve \
  --persist-logins \
  --admin admin &
