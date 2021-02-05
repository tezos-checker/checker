{ ci ? false }:

let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  ligoPkgs =
    # a version slightly newer than 0.9, including the "uncurry" optimisation
    let ligoSrc = pkgs.fetchgit {
      url = "https://gitlab.com/ligolang/ligo";
      rev = "da018e9aee21b612ac8136a3f78bb3728a31ba7a";
      sha256 = "sha256-EqgtQtvFOk+DModB4SOAd+YlBXC/tkuOYMoYtU5WLXU=";
    };
    in (import "${ligoSrc}/nix/pkgs.nix" {});
  tezosClient =
    let tezosPackaging = import "${sources.tezos-packaging}/nix" {};
    in  pkgs.runCommand "tezos-client" {} ''
      mkdir -p $out/bin
      ln -s "${tezosPackaging.binaries.tezos-client.bin}/tezos-client" $out/bin/tezos-client
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
