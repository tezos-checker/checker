{ ci ? false }:

let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ligoPkgs =
    # a version slightly newer than 0.9, including the "uncurry" optimisation
    let ligoSrc = pkgs.fetchgit {
      url = "https://gitlab.com/ligolang/ligo";
      rev = "6a052f5fd45a4300b818169e9d589a609d09632e";
      sha256 = "1685z4gnqavifvivk5k0j2a83k2zj35pbidx4pz0cxpjkh945p6b";
    };
    in (import "${ligoSrc}/nix/pkgs.nix" {});
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
    pkgs.lib.optionals (pkgs.stdenv.isLinux && !ci)
      [ ligoPkgs.ligo
        tezosClient
      ]
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
