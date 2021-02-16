#!/usr/bin/env bash

# This requires having tezos binaries on $PATH. To obtain them, clone & build
# https://gitlab.com/tezos/tezos (install required system dependencies and
#; "make build-deps && make all && make build-sandbox")
#
# Running Tezos now requries some Zcash parameters, which can be obtained with
#  https://raw.githubusercontent.com/zcash/zcash/master/zcutil/fetch-params.sh
# which will download ~2.5 GB's of data.
#
# Detailed logging from the spawned components end up in /tmp/mininet-test/.


set -o errexit
set -o pipefail
set -o xtrace

cd "$(realpath "$(dirname "$0")")"

tezos-sandbox \
  mini-network \
  --number-of-b 1 --time-b 5 \
  --set-history-mode N000:archive --size 1 \
  --add-bootstrap-account "$(tezos-sandbox key alice)@2_000_000_000" \
  --add-bootstrap-account "$(tezos-sandbox key bob)@2_000_000_000" \
  --no-daemons-for alice --no-daemons-for bob \
  --tezos-baker-alpha-binary tezos-baker-008-PtEdoTez \
  --tezos-accuser-alpha-binary tezos-accuser-008-PtEdoTez \
  --tezos-endorser-alpha-binary tezos-endorser-008-PtEdoTez \
  --protocol-kind Edo \
  --protocol-hash PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA
