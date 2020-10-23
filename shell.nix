let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell {
  name = "huxian";
  buildInputs = with pkgs.ocamlPackages; [
    ocaml
    dune
    findlib # Lets merlin see packages like ounit
    ocp-indent
    merlin
    ounit
    qcheck
    ppx_deriving
  ];
}
