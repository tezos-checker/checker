#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

tezos-client originate contract \
  testcontract \
  transferring 1 \
  from bob \
  running generated/michelson/main.tz \
  --init "$(cat generated/michelson/storage.tz)" \
  --no-print-source \
  --burn-cap 25 \
  --force
