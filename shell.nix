let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ligoBinary =
    # Run 'niv update ligo-artifacts -r <git_rev>' to update
    pkgs.runCommand "ligo-binary" { buildInputs = [ pkgs.unzip ]; } ''
      mkdir -p $out/bin
      unzip ${sources.ligo-artifacts} ligo -d $out/bin
    '';
  tezosClient =
    let tezosSrcs = import "${sources.tezos-packaging}/nix/nix/sources.nix";
        patchedSrcs = tezosSrcs // {
          tezos = pkgs.applyPatches
            { name = "tezos-patched";
              src = tezosSrcs.tezos;
              patches = [
                ./patches/max_operation_data_length.patch
                # ./patches/michelson_maximum_type_size.patch
              ];
            };
          opam-nix = pkgs.applyPatches
            { name = "tezos-patched";
              src = tezosSrcs.opam-nix;
              patches = [
                # To fix issue: https://github.com/serokell/opam-nix/issues/3
                (pkgs.fetchpatch {
                   url = "https://github.com/serokell/opam-nix/commit/3d82a0811ec89b3c2e05cfc259cfc5c537560ed6.patch";
                   sha256 = "sha256-MYQk23kRx6r4th61YLmrwXYBLTAjSksgUjy/JzkCaK4=";
                   revert = true;
                })
              ];
            };
        };
        tezosPkgs = import "${sources.tezos-packaging}/nix/build/pkgs.nix" { sources = patchedSrcs; };
    in  pkgs.runCommand "tezos-client" {} ''
      mkdir -p $out/bin
      ln -s \
        "${tezosPkgs.ocamlPackages.tezos-client.bin}/tezos-client" \
        $out/bin/tezos-client
    '';
in
pkgs.mkShell {
  name = "huxian-ocaml";
  buildInputs =
    # ligo does not compile on macos, also we don't want to
    # compile it in CI
    pkgs.lib.optionals (pkgs.stdenv.isLinux)
      [ ligoBinary
        tezosClient
      ]
    ++ [ pkgs.niv ]
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
}
