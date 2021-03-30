let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ligoBinary =
    # Run 'niv update ligo-artifacts -r <git_rev>' to update
    pkgs.runCommand "ligo-binary" { buildInputs = [ pkgs.unzip ]; } ''
      mkdir -p $out/bin
      unzip ${sources.ligo-artifacts} ligo -d $out/bin
      chmod +x $out/bin/ligo
    '';
in
pkgs.mkShell {
  name = "huxian-ocaml";
  buildInputs =
    # ligo does not compile on macos, also we don't want to
    # compile it in CI
    pkgs.lib.optionals (pkgs.stdenv.isLinux)
      [ ligoBinary
      ]
    ++ [ pkgs.niv ]
    ++ (with pkgs.ocamlPackages; [
      ocaml
      ocaml-lsp
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
}
