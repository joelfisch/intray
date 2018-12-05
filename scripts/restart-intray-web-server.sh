#!/bin/bash
set -x

cd $HOME

pkill -f 'intray-web-server serve' || true

set -e

export PORT=8000
export API_PORT=8001

intray-web-server serve --persist-logins --admin admin &
