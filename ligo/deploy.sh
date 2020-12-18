#!/usr/bin/env sh

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

make checker.tz
make storage.tz

tezos-client originate contract \
  testcontract \
  transferring 1 \
  from bob \
  running checker.tz \
  --init "$(cat storage.tz)" \
  --no-print-source \
  --burn-cap 8

