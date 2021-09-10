#!/usr/bin/env bash

set -o xtrace
set -o errexit

FD="fdfind -E vendor/ -E _build/ -E .opam/ -E _opam/"

$FD -e '.ml' -x opam exec -- ocp-indent --inplace
$FD -e '.mli' -x opam exec -- ocp-indent --inplace

new_dune="$(mktemp)"; opam exec -- dune format-dune-file src/dune > "$new_dune" && mv "$new_dune" src/dune
