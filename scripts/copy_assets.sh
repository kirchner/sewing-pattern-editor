#!/usr/bin/env bash

set -e

OUTPUT=$1

cp \
  assets/app.html \
  assets/app.js \
  assets/manifest.webmanifest \
  assets/main.css \
  $OUTPUT

cp \
  assets/images/* \
  assets/fonts/* \
  assets/js-aruco/* \
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
