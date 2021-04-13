#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

cd "$(realpath "$(dirname "$0")")/../"

target_dir="$PWD/generated/michelson"
rm -rf "$target_dir"
mkdir -p "$target_dir"

main="$PWD/generated/ligo/main.mligo"
checkerEntrypoints="$PWD/generated/ligo/checkerEntrypoints.mligo"

echo 'Compiling: main'
ligo compile-contract --warn false "$main" main --output-file "$target_dir/main.tz"
ligo measure-contract --warn false "$main" main

functions=$(perl -n -e'/let lazy_fun_(\S+)/ && print "$1\n"' "$checkerEntrypoints")

for fun in $functions; do
  echo "Packing: $fun"
  prefix="$target_dir/lazy_fun_$fun.tz."
  ligo compile-expression cameligo \
     --warn false \
     --init-file "$main" \
    "Bytes.pack lazy_fun_$fun" \
    | split -b 32000 -d - $prefix

 # Make sure that everything is prefixed by 0x
 sed -z -e 's/^0x//g' -e 's/^/0x/g' -i "$prefix"*

 # Size of endpoints in storage will be almost exactly half the encoded size
 size=$(echo "$(cat "$prefix"*|wc -c) / 2"|bc)
 echo " -> ~$size bytes"
done

echo "done." 1>&2
