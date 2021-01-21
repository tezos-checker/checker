{ ci ? false }:

let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ligoPkgs =
    let ligoSrc = pkgs.fetchzip {
      url = "https://gitlab.com/ligolang/ligo/-/archive/0.8.0/ligo-0.8.0.zip";
      sha256 = "sha256-A6ZsMXhi6QEqgIJubLsZTHcZrTmgnLrdK5ScUdeyc38="; };
    in (import "${ligoSrc}/nix/pkgs.nix" {});
in
pkgs.mkShell {
  name = "huxian-ocaml";
  buildInputs =
    # ligo does not compile on macos, also we don't want to
    # compile it in CI
    pkgs.lib.optional (pkgs.stdenv.isLinux && !ci) ligoPkgs.ligo
    ++ (with pkgs.ocamlPackages; [
      ocaml
      dune_2
      findlib # Lets merlin see packages like ounit
      ocp-indent
      merlin
      ounit
      qcheck
      ppx_deriving
      zarith
      odoc
    ]);
}
