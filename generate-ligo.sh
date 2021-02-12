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
  tokenTypes
  burrowTypes
  constants
  liquidationAuction
  delegationAuction
  permission
  parameters
  uniswap
  burrow
  checkerTypes
  tickets
  checker
)

storage_inputs=(
  ptr
  common
  ratio
  fixedPoint
  kit
  liquidationAuctionTypes
  mem
  avl
  tokenTypes
  burrowTypes
  constants
  liquidationAuction
  delegationAuction
  permission
  parameters
  uniswap
  burrow
  checkerTypes
)

for name in "${inputs[@]}"; do
  from=src/"$name".ml
  to=generated/ligo/"$name".mligo
  echo "$from -> $to" 1>&2

  cat "$from" |
    # comment out the lines between "{BEGIN,END}_OCAML" comments
    sed -E 's/([(]\* BEGIN_OCAML )\*[)]/\1  /g' |
    sed -E 's/[(]\*( END_OCAML \*[)])/  \1/g' |

    # Remove ligo qualifiers from identifiers
    sed -E 's/Ligo\.//g' |
    sed -E 's/LigoOp\.//g' |

    # Comment out 'open's
    sed -E 's/^(open .*)/(* \1 *)/g' |

    # Remove deriving directives
    sed -E 's/([[]@@deriving .*])/(* \1 *)/g' |

    # Remove printer directives
    sed -E 's/([[]@printer .*])/(* \1 *)/g' |

    # Transform inlining directives to ligo-style
    sed -E 's/let([[]@inline])/\1 let/g' |

    # delete assertions
    sed 's/^ *assert.*//g' |

    # replace "_" with ignored
    sed 's/ _ / ignored /g' |
    sed -E 's/([^a-z])_([a-z]+)/\1ignored_\2/g' |

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

    # forget separate kinds of transfers
    sed -E 's/Tezos\.unit_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.address_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.kit_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.lqt_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.da_bid_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.la_bid_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.perm_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.tez_address_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.opt_key_hash_transaction/Tezos\.transaction/g' |
    sed -E 's/Tezos\.tez_transaction/Tezos\.transaction/g' |

    cat > "$to"
done

echo "src/ligo.mligo => generated/ligo.mligo" 2>&1
cp src/ligo.mligo generated/ligo/ligo.mligo

echo "=> main.mligo" 2>&1

echo '#include "ligo.mligo"' > generated/ligo/main.mligo

( IFS=$'\n'; echo "${inputs[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> generated/ligo/main.mligo

# Do everything again to generate the initial storage
echo '#include "ligo.mligo"' > generated/ligo/storagemain.mligo

( IFS=$'\n'; echo "${storage_inputs[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> generated/ligo/storagemain.mligo

echo "done." 1>&2
