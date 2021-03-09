#!/usr/bin/env bash

set -o errexit
set -o pipefail

inputs=(
  error
  ptr
  common
  ratio
  fixedPoint
  kit
  uniswapTypes
  liquidationAuctionPrimitiveTypes
  mem
  avl
  liquidationAuctionTypes
  tickets
  delegationAuctionTypes
  burrowTypes
  constants
  permission
  parameters
  burrow
  checkerTypes
  uniswap
  liquidationAuction
  delegationAuction
  checker
)

for name in "${inputs[@]}"; do
  from="$PWD/src/$name".ml
  to="$PWD/generated/ligo/$name".mligo
  echo "$from -> $to" 1>&2

  cat "$from" |
    # comment out the lines between "{BEGIN,END}_OCAML" comments
    sed -E 's/([(]\* BEGIN_OCAML )\*[)]/\1  /g' |
    sed -E 's/[(]\*( END_OCAML \*[)])/  \1/g' |

    # Replace all "vocal" failwiths with integer failures.
    # TODO: Ideally we wouldn't have this line.
    # User-facing errors should all fail using appropriate error codes (see
    # error.ml) and internal errors should be turned into assertions.
    # If I understand correctly, we currently have internal errors that use
    # failwith, and that's why we replace them with "cheap" (failwith 42) here.
    # Eventually this should go away though.
    sed -E 's/failwith \"([^\"])*\"/Ligo\.failwith 42/g' |

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
    sed 's/^ *assert .*//g' |
    sed 's/^ *assert_burrow_invariants .*//g' |
    sed 's/^ *assert_checker_invariants .*//g' |

    # replace "_" with ignored
    sed 's/ _ / ignored /g' |
    sed -E 's/([^a-z0-9])_([a-z0-9]+)/\1ignored_\2/g' |

    # replace 'int_from_literal' with its argument and replace
    # the double quotes with parentheses (for the potential sign)
    sed -E 's/int_from_literal \"([+-])?([0-9_]+)\"/(\1\2)/g' |

    # replace 'int_from_literal' with its argument and remove the double quotes
    sed -E 's/nat_from_literal \"([0-9_]+)n\"/\1n/g' |

    # replace 'tez_from_literal' with its argument and remove the double quotes
    sed -E 's/tez_from_literal \"([0-9_]+)mutez\"/\1mutez/g' |

    # replace 'address_from_literal' with its argument (keep the double quotes)
    sed -E 's/address_from_literal \"([0-9a-zA-Z]+)\"/\"\1\"/g' |

    # remove the dereferences from Ligo.Tezos values
    sed -E 's/!(Tezos\.)/\1/g' |

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
    sed -E 's/Tezos\.nat_contract_transaction/Tezos\.transaction/g' |

    cat > "$to"
done

echo "$PWD/src/ligo.mligo => $PWD/generated/ligo.mligo" 2>&1
cp "$PWD/src/ligo.mligo" "$PWD/generated/ligo/ligo.mligo"

echo "=> main.mligo" 2>&1

echo '#include "ligo.mligo"' > "$PWD/generated/ligo/main.mligo"

( IFS=$'\n'; echo "${inputs[*]}" ) |
  sed -E 's/(.*)/#include "\1.mligo"/g' |
  cat >> "$PWD/generated/ligo/main.mligo"

echo "done." 1>&2
