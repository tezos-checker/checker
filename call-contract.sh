#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

arg="$1"

tezos-client call testcontract from bob \
  --arg "$(ligo compile-expression --protocol edo --init-file generated/ligo/storage.mligo cameligo "$arg")" \
  --burn-cap 100
