#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

cd "$(realpath "$(dirname "$0")")/../"

target_dir="$PWD/generated/michelson"
rm -rf "$target_dir"
mkdir -p "$target_dir"

main="$PWD/generated/ligo/main.mligo"
checker="$PWD/generated/ligo/checker.mligo"

echo 'Compiling: main'
./bin/ligo compile-contract --warn false "$main" main --output-file "$target_dir/main.tz"
./bin/ligo measure-contract --warn false "$main" main

functions=( $(grep -o 'let fun_[^ ]*' "$checker" | sed -E 's/.{8}(.*)/\1/g' ) )

for fun in "${functions[@]}"; do
  echo "Packing: $fun"
  ./bin/ligo compile-expression cameligo \
     --warn false \
     --init-file "$main" \
    "Bytes.pack $fun" \
    | split -b 40000 -d - "$target_dir/fun_$fun.tz."
done

echo "done." 1>&2
