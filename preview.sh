#!/usr/bin/env bash

set -e

./build.sh

ENVIRONMENT="production" PORT=1234 go run server.go
