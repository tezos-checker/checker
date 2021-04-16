#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

michelson="$PWD/generated/michelson"
functions_json="$(cat "$michelson"/functions.json)"

self_address="$(tezos-client show address bob | grep Hash | cut -d ' ' -f 2)"

storage_template="$(echo "$functions_json" | jq -r .initial_storage)"
storage_placeholder="$(echo "$functions_json" | jq -r .initial_storage_address_placeholder)"
storage="$(echo "$storage_template" | sed "s/$storage_placeholder/$self_address/")"

echo '========================'
echo 'Deploying main contract'
echo '========================'
# tezos-client originate contract \
  # testcontract \
  # transferring 1 \
  # from bob \
  # running "$PWD/generated/michelson/main.tz" \
  # --init "$storage" \
  # --no-print-source \
  # --burn-cap 25 \
  # --force

transfers=$(mktemp --suffix="-transfers.json")
echo "$functions_json"  \
  | jq '.lazy_functions | '
exit 1

echo "[ { \"destination\": \"testcontract\", \"amount\": \"0\", \"entrypoint\": \"sealContract\" }
      , { \"destination\": \"testcontract\", \"amount\": \"0\", \"entrypoint\": \"sealContract\" }  ]" > "$transfers"

tezos-client multiple transfers from bob using "$transfers"

# for fname in generated/michelson/lazy_fun_*.tz*; do
#   echo '========================'
#   echo "Deploying $fname"
#   echo '========================'
#
#   fun="$(echo "$fname" | sed -E 's|.*/lazy_fun_(.*).tz.*|\1|g')"
#   contents="$(cat "$fname" | sed -z -e 's/^0x//g' -e 's/^/0x/g')"
#
#   param="$(ligo compile-parameter --warn false "$main" main "DeployFunction (lazy_id_$fun, $contents)")"
#   tezos-client call testcontract from bob \
#     --arg "$param" \
#     --burn-cap 25 \
#     --no-print-source
# done
#
# echo '========================'
# echo "Sealing"
# echo '========================'
# param="$(ligo compile-parameter --warn false "$main" main "SealContract")"
# tezos-client call testcontract from bob \
#   --arg "$param" \
#   --burn-cap 25 \
#   --no-print-source
#
# echo '========================'
# echo "Done."
# echo '========================'
