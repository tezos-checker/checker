let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ligoBinary =
    # Run 'niv update ligo-artifacts -r <git_rev>' to update
    pkgs.runCommand "ligo-binary" { buildInputs = [ pkgs.unzip ]; meta.platforms = [ "x86_64-linux" ];  } ''
      mkdir -p $out/bin
      unzip ${sources.ligo-artifacts} ligo -d $out/bin
      chmod +x $out/bin/ligo
    '';
in
pkgs.stdenv.mkDerivation {
  name = "huxian";
  buildInputs =
    [ ligoBinary pkgs.niv pkgs.bash pkgs.coreutils ]
    ++ (with pkgs.ocamlPackages; [
      ocaml
      dune_2
      findlib # Lets merlin see packages like ounit
      ocp-indent
      merlin
      ounit
      qcheck
      ppxlib
      bisect_ppx
      ppx_tools_versioned
      ppx_deriving
      zarith
      odoc
    ]);
  src = ./.;
  buildPhase = ''
    patchShebangs --build ./scripts/generate-ligo.sh
    make
  '';
  installPhase = ''
    mv generated $out/
  '';
}
