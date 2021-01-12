#!/usr/bin/env sh

set -o xtrace
set -o errexit
set -o pipefail

cd "$(realpath "$(dirname "$0")")"

cat ../src/avl.ml |

  # delete all lines with preprocessor directives ([@@deriving])
  sed 's/^\[@@.*//g' |

  # delete assertions
  grep -vE '^ *assert' |

  # replace type parameters with concrete types
  sed "s/'l/liquidation_slice/g" |
  sed "s/'r/auction_outcome/g" |

  # monomorphise higher kinded leaf type
  sed "s/liquidation_slice leaf/leaf/g" |

  # monomorphise higher kinded node type
  sed 's/[(][^()]*[)] node/node/g' |

  # monomorphise higher kinded mem type
  sed 's/[(][^()]*[)] mem/mem/g' |

  # set mem to correct type
  sed 's/^type mem =.*/type mem = (int, node) big_map/g' |

  # remove module scoping for Tez
  sed -E 's/Tez\.[(]([^)]*)[)]/\1/g' |

  # remove the lines after "Test utilities" header
  sed '/[(]* Test utilities */,$d' |

  # replace "_" with ignored
  sed 's/ _ / ignored /g' |

  # write the result
  cat > avl.mligo
