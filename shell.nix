let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  ligoPkgs =
    let ligoSrc = pkgs.fetchgit {
      url = "https://gitlab.com/ligolang/ligo";
      sha256 = "sha256-VfqXbL8CZDDSqLmp9wOUSaszB1OcCj4N6QVf2VzoTiA="; };
    in (import "${ligoSrc}/nix/pkgs.nix" {});
in
pkgs.mkShell {
  name = "huxian";
  buildInputs = with pkgs.ocamlPackages; [
    ocaml
    dune_2
    findlib # Lets merlin see packages like ounit
    ocp-indent
    merlin
    ounit
    qcheck
    ppx_deriving
    zarith
    ligoPkgs.ligo-bin
  ];
}
