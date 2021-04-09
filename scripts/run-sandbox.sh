#!/usr/bin/env bash

# Follow:
# https://assets.tqtezos.com/docs/setup/2-sandbox/

set -o errexit
set -o pipefail

set -o xtrace

docker run -i -t --rm \
  --name tezos-sandbox \
  -p 20000:20000 \
  tqtezos/flextesa:20210316 \
  flobox info
docker run -i -t --rm \
  --name tezos-sandbox \
  -p 20000:20000 \
  tqtezos/flextesa:20210316 \
  flobox start
