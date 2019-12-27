#!/bin/bash
set -x

cd $HOME

pkill -f 'intray-web-server serve' || true

set -e

intray-web-server serve \
  --persist-logins \
  --admin admin &
