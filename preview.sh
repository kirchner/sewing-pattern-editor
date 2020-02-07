#!/usr/bin/env bash

set -e

./build.sh

source ./secrets.sh

cabal run run-server -- \
  --environment=production \
  --port=1234 \
  --clientid=$CLIENT_ID \
  --clientsecret=$CLIENT_SECRET
