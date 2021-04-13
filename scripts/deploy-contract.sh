#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

main="$PWD/generated/ligo/main.mligo"

self_address="$(tezos-client show address bob | grep Hash | cut -d ' ' -f 2)"
storage="$(ligo compile-storage --warn false \
  --now='2021-01-01T10:10:10Z' \
  "$main" \
  main \
  "initial_wrapper (\"$self_address\": address)"
)"

echo '========================'
echo 'Deploying main contract'
echo '========================'
tezos-client originate contract \
  testcontract \
  transferring 1 \
  from bob \
  running "$PWD/generated/michelson/main.tz" \
  --init "$storage" \
  --no-print-source \
  --burn-cap 25 \
  --force

for fname in generated/ligo_compiled/*.bc; do
  echo '========================'
  echo "Deploying $fname"
  echo '========================'

  param="$(cat $fname)"
  tezos-client call testcontract from bob \
    --arg "$param" \
    --burn-cap 25 \
    --no-print-source
done

echo '========================'
echo "Sealing"
echo '========================'
param="$(ligo compile-parameter --warn false "$main" main "SealContract")"
tezos-client call testcontract from bob \
  --arg "$param" \
  --burn-cap 25 \
  --no-print-source

echo '========================'
echo "Done."
echo '========================'
