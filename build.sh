#!/usr/bin/env bash

set -e

OUTPUT="./_build"

ELM="./src/Main.elm"
JS="${OUTPUT}/elm.js"
JS_MIN="${OUTPUT}/elm.min.js"

yarn elm make $ELM \
  --optimize \
  --output=$JS

yarn jscodeshift -t transform.js $JS

yarn uglifyjs $JS \
  --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
  --output=$JS_MIN

yarn uglifyjs $JS_MIN \
  --mangle \
  --output=$JS_MIN

yarn prepack $JS_MIN --inlineExpressions --out $JS_MIN

echo "Compiled size: $(cat $JS | wc -c) bytes  ($JS)"
echo "Minified size: $(cat $JS_MIN | wc -c) bytes  ($JS_MIN)"
echo "Gzipped size:  $(cat $JS_MIN | gzip -c | wc -c) bytes"

mv $JS_MIN $JS

cp \
  service-worker.js \
  register-service-worker.js \
  $OUTPUT

./copy_assets.sh $OUTPUT
