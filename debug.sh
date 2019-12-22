#!/usr/bin/env bash

set -e

OUTPUT="./_debug"

ELM="src/Main.elm"
JS="${OUTPUT}/elm.js"

yarn elm make $ELM \
  --debug \
  --output=$JS

echo "Compiled size: $(cat $JS | wc -c) bytes  ($JS)"

cp \
  register-service-worker--no-op.js \
  "${OUTPUT}/register-service-worker.js"

./copy_assets.sh $OUTPUT

ENVIRONMENT="development" PORT=4321 go run server.go
