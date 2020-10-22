let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ocamlPackages = pkgs.ocamlPackages;
in
pkgs.mkShell {
  name = "huxian";
  buildInputs = [
    ocamlPackages.ocaml
    ocamlPackages.dune
    ocamlPackages.findlib # Lets merlin see packages like ounit
    ocamlPackages.ocp-indent
    ocamlPackages.merlin
    ocamlPackages.ounit
    ocamlPackages.ppx_deriving
  ];
}
