let
sources = import ./nix/sources.nix {};
pkgs = import sources.nixpkgs {};
ocamlPackages = pkgs.ocamlPackages;
in
pkgs.mkShell {
  name = "huxian";
  buildInputs = [
    ocamlPackages.ocaml
    ocamlPackages.ocp-indent
    ocamlPackages.merlin
    ocamlPackages.ocamlbuild
  ];
}
