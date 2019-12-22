#!/usr/bin/env bash

set -e

ELM="src/Main.elm"
JS="./dist/elm.js"
JS_MIN="./dist/elm.min.js"

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

cp index.html dist/index.html
cp manifest.webmanifest dist/manifest.webmanifest
cp main.css dist/main.css
cp service-worker.js dist/service-worker.js

cp assets/icon-32.png dist/icon-32.png
cp assets/icon-64.png dist/icon-64.png
cp assets/icon-128.png dist/icon-128.png
cp assets/icon-256.png dist/icon-256.png
cp assets/icon-512.png dist/icon-512.png

cp assets/rubik-v9-latin-300.woff dist/rubik-v9-latin-300.woff
cp assets/rubik-v9-latin-300.woff2 dist/rubik-v9-latin-300.woff2
cp assets/mansalva-v1-latin-regular.woff dist/mansalva-v1-latin-regular.woff
cp assets/mansalva-v1-latin-regular.woff2 dist/mansalva-v1-latin-regular.woff2

mkdir -p dist/css
mkdir -p dist/webfonts
cp node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css dist/css/
cp node_modules/@fortawesome/fontawesome-free/css/solid.min.css dist/css/
cp node_modules/@fortawesome/fontawesome-free/webfonts/fa-solid-900.* dist/webfonts/
