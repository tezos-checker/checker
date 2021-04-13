#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

set -o xtrace

arg="$1"

tezos-client call testcontract from bob \
  --arg "$(ligo compile-parameter --warn false --protocol edo $PWD/generated/ligo/main.mligo main "CheckerEntrypoint ($arg)")" \
  --burn-cap 100
