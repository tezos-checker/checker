#!/usr/bin/env bash

set -o errexit
set -o pipefail

cd "$(realpath "$(dirname "$0")")"

inputs=(
  ptr
  common
  ratio
  fixedPoint
  kit
  liquidationAuctionTypes
  mem
  avl
  constants
  liquidationAuction
  delegationAuction
  permission
  parameters
  uniswap
  burrow
)

for name in "${inputs[@]}"; do
  from=src/"$name".ml
  to=generated/ligo/"$name".mligo
  echo "$from -> $to" 1>&2

  cat "$from" |
    # comment out the lines between "{BEGIN,END}_OCAML" comments
    sed -E 's/([(]\* BEGIN_OCAML )\*[)]/\1  /g' |
    sed -E 's/[(]\*( END_OCAML \*[)])/  \1/g' |

    # Remove ligo qualifier from identifiers
    sed -E 's/Ligo\.//g' |

    # Comment out 'open's
    sed -E 's/^(open .*)/(* \1 *)/g' |

    # Remove deriving directives
    sed -E 's/([[]@@deriving .*])/(* \1 *)/g' |

    # Remove printer directives
    sed -E 's/([[]@printer .*])/(* \1 *)/g' |

    # delete assertions
    sed 's/^ *assert.*//g' |

    # replace "_" with ignored
    sed 's/ _ / ignored /g' |

    # replace 'int_from_literal' with its argument and replace
    # the double quotes with parentheses (for the potential sign)
    sed -E 's/int_from_literal \"([+-])?([0-9_]+)\"/(\1\2)/g' |

    # replace 'int_from_literal' with its argument and remove the double quotes
    sed -E 's/nat_from_literal \"([0-9_]+)n\"/\1n/g' |

    # replace 'tez_from_literal' with its argument and remove the double quotes
    sed -E 's/tez_from_literal \"([0-9_]+)mutez\"/\1mutez/g' |

    # remove the dereferences from Ligo.Tezos values
    sed -E 's/!(Tezos\..*)/\1/g' |
    sed -E 's/!(tezos_level)/\1/g' |

    cat > "$to"
done

echo "src/ligo.mligo => generated/ligo.mligo" 2>&1
cp src/ligo.mligo generated/ligo/ligo.mligo

echo "=> main.mligo" 2>&1

echo '#include "ligo.mligo"' > generated/ligo/main.mligo

( IFS=$'\n'; echo "${inputs[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> generated/ligo/main.mligo

cat <<EOF >> generated/ligo/main.mligo
let main (i, storage: int * int): operation list * int =
  (([]: operation list), i+1)
EOF

echo "done." 1>&2
