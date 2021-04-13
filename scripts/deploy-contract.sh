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

for fname in generated/michelson/lazy_fun_*.tz*; do
  echo '========================'
  echo "Deploying $fname"
  echo '========================'

  fun="$(echo "$fname" | sed -E 's|.*/lazy_fun_(.*).tz.*|\1|g')"
  contents="$(cat "$fname" | sed -z -e 's/^0x//g' -e 's/^/0x/g')"

  param="$(ligo compile-parameter --warn false "$main" main "DeployFunction (lazy_id_$fun, $contents)")"
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
