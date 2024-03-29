#!/usr/bin/env bash

set -o errexit
set -o pipefail

cd "$(realpath "$(dirname "$0")")/../"

target_dir="$PWD/generated/ligo"
rm -rf "$target_dir"
mkdir -p "$target_dir"

# Note: order here does matter since it affects the order of #includes in main.mligo
checker_sources=(
  error
  fa12Interface
  ptr
  common
  fixedPoint
  fa2Interface
  tokenMetadata
  ctok
  kit
  lqt
  tok
  fa2Ledger
  fa2Implementation
  cfmmTypes
  liquidationAuctionPrimitiveTypes
  mem
  avl
  liquidationAuctionTypes
  burrowTypes
  vaultTypes
  constants
  driftDerivative
  targetCalculation
  parameters
  burrow
  checkerTypes
  cfmm
  sliceList
  liquidationAuction
  burrowOrigination
  price
  getOracleEntrypoint
  checker
  checkerEntrypoints
  checkerMain
)

# Note: order here does matter since it affects the order of #includes in wtezMain.mligo
wtez_sources=(
  error
  common
  fa2Interface
  tokenMetadata
  fa2Ledger
  vaultTypes
  wtez
)

# Note: order here does matter since it affects the order of #includes in wctezMain.mligo
wctez_sources=(
  error
  fa12Interface
  common
  fa2Interface
  tokenMetadata
  fa2Ledger
  wctez
)

# Note: order here does matter since it affects the order of #includes in mockFA2.mligo
mock_fa2_sources=(
  error
  common
  fa2Interface
  tokenMetadata
  fa2Ledger
  mockFA2
)

all_sources=( "${checker_sources[@]}" "${wtez_sources[@]}" "${wctez_sources[@]}" "${mock_fa2_sources[@]}" )
all_sources=($(echo "${all_sources[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))

for name in "${all_sources[@]}"; do
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

# Generate the Checker contract
echo "=> main.mligo" 2>&1

echo '#include "ligo.mligo"' > "$target_dir/main.mligo"

( IFS=$'\n'; echo "${checker_sources[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> "$target_dir/main.mligo"

# Generate the wtez contract
echo "=> wtezMain.mligo" 2>&1

echo '#include "ligo.mligo"' > "$target_dir/wtezMain.mligo"

( IFS=$'\n'; echo "${wtez_sources[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> "$target_dir/wtezMain.mligo"

# Generate the wctez contract
echo "=> wctezMain.mligo" 2>&1

echo '#include "ligo.mligo"' > "$target_dir/wctezMain.mligo"

( IFS=$'\n'; echo "${wctez_sources[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> "$target_dir/wctezMain.mligo"

# Generate the mockFA2 contract
echo "=> mockFA2Main.mligo" 2>&1

echo '#include "ligo.mligo"' > "$target_dir/mockFA2Main.mligo"

( IFS=$'\n'; echo "${mock_fa2_sources[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> "$target_dir/mockFA2Main.mligo"

echo "done." 1>&2
