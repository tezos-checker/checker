#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

main="$PWD/generated/ligo/main.mligo"

mkdir -p generated/ligo_compiled

for fname in generated/michelson/lazy_fun_*.tz*; do
  echo '========================'
  echo "Compiling $fname"
  echo '========================'

  fun="$(echo "$fname" | sed -E 's|.*/lazy_fun_(.*).tz.*|\1|g')"
  contents="$(cat "$fname" | sed -z -e 's/^0x//g' -e 's/^/0x/g')"

  ligo compile-parameter --warn false "$main" main "DeployFunction (lazy_id_$fun, $contents)" > generated/ligo_compiled/$fun.bc
done
