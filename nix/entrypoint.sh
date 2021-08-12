#!/bin/sh

NIX_SH="$HOME/.nix-profile/etc/profile.d/nix.sh"
source "$NIX_SH"

if ! [[ -f /mnt/shell.nix ]]; then
  echo "Mount a checker repository under /mnt/ for this image to work." >&2
  exit 1
fi

nix-shell /mnt/shell.nix --run "$@"
