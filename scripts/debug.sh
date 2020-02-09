#!/usr/bin/env bash

set -e

OUTPUT="_debug"

ELM="src/frontend/Main.elm"
JS="${OUTPUT}/elm.js"

yarn elm make $ELM \
  --debug \
  --output=$JS

echo "Compiled size: $(cat $JS | wc -c) bytes  ($JS)"

cp \
  assets/register-service-worker--no-op.js \
  "${OUTPUT}/register-service-worker.js"

scripts/copy_assets.sh $OUTPUT

source secrets.sh

cabal run run-server -- \
  --environment=development \
  --port=4321 \
  --clientid=$CLIENT_ID \
  --clientsecret=$CLIENT_SECRET