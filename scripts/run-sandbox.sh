#!/usr/bin/env bash

# This requires having tezos binaries on $TEZOS_BIN. To obtain them, clone & build
# https://gitlab.com/tezos/tezos (install required system dependencies and
# "make build-deps && make all && make build-sandbox")
#
# Running Tezos now requries some Zcash parameters, which can be obtained with
#  https://raw.githubusercontent.com/zcash/zcash/master/zcutil/fetch-params.sh
# which will download ~2.5 GB's of data.
#
# Detailed logging from the spawned components end up in /tmp/mininet-test/.


set -o errexit
set -o pipefail

if [[ -z "$TEZOS_BIN" ]]; then
  echo 'Missing $TEZOS_BIN' 2>&1
  exit 1
fi

set -o xtrace
PATH="$TEZOS_BIN:$PATH" "$TEZOS_BIN/tezos-sandbox" \
  mini-network \
  --number-of-b 1 --time-b 5 \
  --set-history-mode N000:archive --size 1 \
  --add-bootstrap-account "$("$TEZOS_BIN/tezos-sandbox" key alice)@2_000_000_000" \
  --add-bootstrap-account "$("$TEZOS_BIN/tezos-sandbox" key bob)@2_000_000_000" \
  --no-daemons-for alice --no-daemons-for bob \
  --tezos-baker-alpha-binary "$TEZOS_BIN/tezos-baker-008-PtEdo2Zk" \
  --tezos-accuser-alpha-binary "$TEZOS_BIN/tezos-accuser-008-PtEdo2Zk" \
  --tezos-endorser-alpha-binary "$TEZOS_BIN/tezos-endorser-008-PtEdo2Zk" \
  --protocol-kind Edo \
  --protocol-hash PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA
