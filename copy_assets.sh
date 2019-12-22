#!/usr/bin/env bash

set -e

OUTPUT=$1

cp \
  app.html \
  manifest.webmanifest \
  main.css \
  app.js \
  $OUTPUT

cp \
  assets/icon-32.png \
  assets/icon-64.png \
  assets/icon-128.png \
  assets/icon-256.png \
  assets/icon-512.png \
  assets/rubik-v9-latin-300.woff \
  assets/rubik-v9-latin-300.woff2 \
  assets/mansalva-v1-latin-regular.woff \
  assets/mansalva-v1-latin-regular.woff2 \
  $OUTPUT

mkdir -p \
  "${OUTPUT}/css" \
  "${OUTPUT}/webfonts"

cp \
  node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css \
  node_modules/@fortawesome/fontawesome-free/css/solid.min.css \
  "${OUTPUT}/css"
cp \
  node_modules/@fortawesome/fontawesome-free/webfonts/fa-solid-900.* \
  "${OUTPUT}/webfonts"
