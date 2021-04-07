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
ligo compile-contract --warn false "$main" main --output-file "$target_dir/main.tz"
ligo measure-contract --warn false "$main" main

functions=$(perl -n -e'/let lazy_fun_(\S+)/ && print "$1\n"' "$checker")

for fun in $functions; do
  echo "Packing: $fun"
  ligo compile-expression cameligo \
     --warn false \
     --init-file "$main" \
    "Bytes.pack $fun" \
    | split -b 40000 -d - "$target_dir/lazy_fun_$fun.tz."
done

echo "done." 1>&2
