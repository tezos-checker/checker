#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

arg="$1"

tezos-client call testcontract from bob \
  --arg "$(ligo compile-parameter --protocol edo $PWD/generated/ligo/main.mligo main "$arg")" \
  --burn-cap 100
