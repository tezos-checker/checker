let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
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
  ];
}
