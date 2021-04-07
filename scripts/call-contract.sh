#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

if [[ -z "$TEZOS_BIN" ]]; then
  echo 'Missing $TEZOS_BIN' 2>&1
  exit 1
fi

set -o xtrace

arg="$1"

"$TEZOS_BIN"/tezos-client call testcontract from bob \
  --arg "$(ligo compile-parameter --protocol edo $PWD/generated/ligo/main.mligo main "$arg")" \
  --burn-cap 100
