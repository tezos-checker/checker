#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

if [[ -z "$TEZOS_BIN" ]]; then
  echo 'Missing $TEZOS_BIN' 2>&1
  exit 1
fi

main="$PWD/generated/ligo/main.mligo"

self_address="$($TEZOS_BIN/tezos-client show address bob | grep Hash | cut -d ' ' -f 2)"
storage="$(ligo compile-storage --warn false \
  --now='2021-01-01T10:10:10Z' \
  "$main" \
  main \
  "initial_wrapper (\"$self_address\": address)"
)"

echo '========================'
echo 'Deploying main contract'
echo '========================'
"$TEZOS_BIN"/tezos-client originate contract \
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

  param="$(ligo compile-parameter "$main" main "DeployFunction (lazy_fun_$fun, $contents)")"
  "$TEZOS_BIN"/tezos-client call testcontract from bob \
    --arg "$param" \
    --burn-cap 25 \
    --no-print-source
done

echo '========================'
echo "Sealing"
echo '========================'
param="$(ligo compile-parameter "$main" main "SealContract")"
"$TEZOS_BIN"/tezos-client call testcontract from bob \
  --arg "$param" \
  --burn-cap 25 \
  --no-print-source

echo '========================'
echo "Done."
echo '========================'
