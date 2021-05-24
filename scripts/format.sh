#!/usr/bin/env bash

set -o xtrace
set -o errexit

FD="fd -E vendor/"

$FD -e '.ml' -x ocp-indent --inplace
$FD -e '.mli' -x ocp-indent --inplace
$FD -e '.nix' -x nixpkgs-fmt
$FD -e '.py' -x black

new_dune="$(mktemp)"; dune format-dune-file src/dune > "$new_dune" && mv "$new_dune" src/dune
new_dune="$(mktemp)"; dune format-dune-file tests/dune > "$new_dune" && mv "$new_dune" tests/dune
