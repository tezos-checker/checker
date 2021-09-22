#!/usr/bin/env bash

set -o errexit
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

target_dir="$PWD/generated/ligo"
rm -rf "$target_dir"
mkdir -p "$target_dir"

# Note: order here does matter since it affects the order of #includes in main.mligo
inputs=(
  error
  fa12Interface
  ptr
  common
  fixedPoint
  ctez
  kit
  lqt
  tok
  cfmmTypes
  fa2Interface
  liquidationAuctionPrimitiveTypes
  mem
  avl
  liquidationAuctionTypes
  burrowTypes
  vaultTypes
  constants
  parameters
  burrow
  checkerTypes
  cfmm
  sliceList
  liquidationAuction
  checker
  checkerEntrypoints
  checkerMain
)

for name in "${inputs[@]}"; do
  from="$PWD/src/$name".ml
  to="$target_dir/$name".mligo
  echo "$from -> $to" 1>&2

  cat "$from" |
    # comment out the lines between "{BEGIN,END}_OCAML" comments
    sed -E 's/([(]\* BEGIN_OCAML )\*[)]/\1  /g' |
    sed -E 's/[(]\*( END_OCAML \*[)])/  \1/g' |

    # remove comments wrapped with "{BEGIN,END}_LIGO"
    sed -E 's/([(]\* BEGIN_LIGO)/\1 *)/g' |
    sed -E 's/(END_LIGO \*[)])/(* \1/g' |

    # Remove ligo qualifiers from identifiers
    sed -E 's/Ligo\.//g' |
    sed -E 's/LigoOp\.//g' |

    # Comment out 'open's
    sed -E 's/^(open .*)/(* \1 *)/g' |

    # Remove deriving directives
    sed -E 's/([[]@@deriving .*])/(* \1 *)/g' |

    # Remove coverage directives
    sed -E 's/([[]@coverage .*])/(* \1 *)/g' |
    sed -E 's/([[]@@coverage .*])/(* \1 *)/g' |
    sed -E 's/([[]@@@coverage .*])/(* \1 *)/g' |

    # Remove printer directives
    sed -E 's/([[]@printer .*])/(* \1 *)/g' |

    # Transform inlining directives to ligo-style
    sed -E 's/let([[]@inline])/\1 let/g' |

    # delete assertions
    sed 's/^ *assert .*//g' |
    sed 's/^ *assert_checker_invariants .*//g' |
    sed 's/^ *assert_liquidation_auction_invariants .*//g' |
    sed 's/^ *assert_burrow_slices_invariants .*//g' |

    # replace 'int_from_literal' with its argument and replace
    # the double quotes with parentheses (for the potential sign)
    sed -E 's/int_from_literal \"([+-])?([0-9_]+)\"/(\1\2)/g' |

    # replace 'int_from_literal' with its argument and remove the double quotes
    sed -E 's/nat_from_literal \"([0-9_]+)n\"/\1n/g' |

    # replace 'tez_from_literal' with its argument and remove the double quotes
    sed -E 's/tez_from_literal \"([0-9_]+)mutez\"/\1mutez/g' |

    # replace 'address_from_literal' with its argument (keep the double quotes)
    sed -E 's/address_from_literal \"([0-9a-zA-Z]+)\"/\"\1\"/g' |

    # replace 'bytes_from_literal' with its argument and remove the double quotes
    sed -E 's/bytes_from_literal \"([^"]+)\"/\1/g' |

    # remove the dereferences from Ligo.Tezos values
    sed -E 's/!(Tezos\.)/\1/g' |

    # map specialized 'Tezos.*_transaction' functions to the generic one
    sed -E 's/Tezos\.([0-9a-zA-Z_]+)_transaction/Tezos\.transaction/g' |

    # map specialized 'Tezos.*_create_contract' functions to the generic one
    sed -E 's/Tezos\.([0-9a-zA-Z_]+)_create_contract/Tezos\.create_contract/g' |

    cat > "$to"
done

echo "$PWD/src/ligo.mligo => $target_dir/ligo.mligo" 2>&1
cp "$PWD/src/ligo.mligo" "$target_dir/ligo.mligo"

echo "=> main.mligo" 2>&1

echo '#include "ligo.mligo"' > "$target_dir/main.mligo"

( IFS=$'\n'; echo "${inputs[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> "$target_dir/main.mligo"

echo "done." 1>&2
