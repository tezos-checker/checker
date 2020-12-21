let
  sources = import ../nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ligoPkgs =
    let ligoSrc = pkgs.fetchzip {
      url = "https://gitlab.com/ligolang/ligo/-/archive/0.6.0/ligo-0.6.0.zip";
      sha256 = "sha256-8apu4qIADhRTxjab8CuYPmwh8RPuoQ34kSfIk3VRE6k="; };
    in (import "${ligoSrc}/nix/pkgs.nix" {});
in
pkgs.mkShell {
  name = "huxian-ligo";
  buildInputs = with pkgs.ocamlPackages; [
    ligoPkgs.ligo-bin
  ];
}
