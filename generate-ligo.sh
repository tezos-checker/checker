#!/usr/bin/env bash

set -o errexit
set -o pipefail

cd "$(realpath "$(dirname "$0")")"

inputs=(
  ptr
)

cp src/ligo.mligo generated/ligo/ligo.mligo

for name in "${inputs[@]}"; do
  from=src/"$name".ml
  to=generated/ligo/"$name".mligo
  echo "$from -> $to" 1>&2

  echo '#include "ligo.mligo"' > "$to"

  cat "$from" |

    # comment out the lines between "{BEGIN,END}_OCAML" comments
    sed -E 's/([(]\* BEGIN_OCAML )\*[)]/\1  /g' |
    sed -E 's/[(]\*( END_OCAML \*[)])/  \1/g' |

    # Remove ligo qualifier from identifiers
    sed -E 's/Ligo\.//g' |

    cat >> "$to"
done

echo "=> main.mligo" 2>&1
( IFS=$'\n'; echo "${inputs[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat > generated/ligo/main.mligo

cat <<EOF >> generated/ligo/main.mligo
#include "ligo.mligo"

let main (i, storage: int * int): operation list * int =
  (([]: operation list), i+1)
EOF

echo "done." 1>&2
