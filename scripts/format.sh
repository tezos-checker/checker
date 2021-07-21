#!/usr/bin/env bash

set -o xtrace
set -o errexit

FD="fd -E vendor/"

$FD -e '.ml' -x ocamlformat --inplace --ocp-indent-compat
$FD -e '.mli' -x ocamlformat --inplace --ocp-indent-compat
$FD -e '.nix' -x nixpkgs-fmt
$FD -e '.py' -x black

new_dune="$(mktemp)"; dune format-dune-file src/dune > "$new_dune" && mv "$new_dune" src/dune
