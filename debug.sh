#!/usr/bin/env bash

set -e

ELM="src/Main.elm"
JS="./dist/elm.js"

yarn elm make $ELM \
  --debug \
  --output=$JS

echo "Compiled size: $(cat $JS | wc -c) bytes  ($JS)"

cp app.html dist/app.html
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
