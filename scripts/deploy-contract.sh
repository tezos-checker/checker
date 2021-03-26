#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

cd "$(realpath "$(dirname "$0")")/../"

tezos-client originate contract \
  testcontract \
  transferring 1 \
  from bob \
  running "$PWD/generated/michelson/main.tz" \
  --init "$(cat $PWD/generated/michelson/storage.tz)" \
  --no-print-source \
  --burn-cap 25 \
  --force
