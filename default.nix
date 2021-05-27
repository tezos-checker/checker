{ doCheck ? false }:

let
  sources = import ./nix/sources.nix { };

  pkgsHost = import sources.nixpkgs {};
  pkgsLinux = import sources.nixpkgs { system = "x86_64-linux"; };

  gitignoreNix = import sources."gitignore.nix" { lib = pkgsHost.lib; };

  ligoBinary =
    # This is a precompiled file, which is the ligo revision `0b2a8d2e3` compiled
    # with the patch ./patches/ligo_michelson_maximum_type_size.patch.
    pkgsLinux.runCommand "ligo-binary" { buildInputs = [ pkgsLinux.unzip ]; } ''
      mkdir -p $out/bin
      ln -s ${./bin/ligo} $out/bin/ligo
    '';
    # Run 'niv update ligo-artifacts -r <git_rev>' to update
    # pkgsLinux.runCommand "ligo-binary" { buildInputs = [ pkgs.unzip ]; } ''
    #   mkdir -p $out/bin
    #   unzip ${sources.ligo-artifacts} ligo -d $out/bin
    #   chmod +x $out/bin/ligo
    # '';

  ocamlDeps = pkgs: with pkgs.ocamlPackages; [
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
     core_kernel
  ];

  # We wrap the upstream tezos-client derivation since it also brings some ocaml
  # packages which are incompatible with ours.
  tezosClient =
    let orig = (import "${sources.tezos-packaging}/nix" { }).binaries.tezos-client;
    in pkgsLinux.runCommand "tezos-client" {} ''
      mkdir -p $out/bin; ln -s ${orig}/bin/tezos-client $out/bin/tezos-client
    '';

  checkerSource =
    let filter =
          let ignored = gitignoreNix.gitignoreFilter ./.;
          in  path: type: ignored path type && builtins.baseNameOf path != "ligo";
    in pkgsHost.lib.cleanSourceWith {
      inherit filter;
      src = ./.;
      name = "checker-source";
    };
in rec
{
  michelson =
    let pkgs = pkgsLinux;
    in pkgs.stdenv.mkDerivation {
         name = "checker-michelson";
         buildInputs = [ ligoBinary ] ++ (with pkgs; [ ruby ]) ++ ocamlDeps pkgs;
         src = checkerSource;
         buildPhase = ''
           export HOME=$(mktemp -d)
           make build-ligo
         '';

         inherit doCheck;
         checkPhase = ''
           make build-ocaml
           make test
         '';
         installPhase = ''
           mkdir -p $out
           cp generated/michelson/* $out
         '';
       };

  spec =
    let pkgs = pkgsHost;
    in pkgs.stdenv.mkDerivation {
      src = checkerSource;
      name = "checker-spec";
      buildInputs = with pkgs; [ sphinx python3Packages.sphinx_rtd_theme ];
      buildPhase = ''
        make spec
      '';
      installPhase = ''
        cp -R docs/spec/_build/html $out
      '';
    };

  shell =
    let pkgs = pkgsHost;
        pythonDeps = import ./nix/python.nix { inherit pkgs; };
    in pkgs.mkShell {
         name = "checker-shell";
         buildInputs =
           # ligo does not compile on macos, also we don't want to
           # compile it in CI
           pkgs.lib.optionals (pkgsHost.stdenv.isLinux) [ ligoBinary ]
           ++ pkgs.lib.optionals (pkgsHost.stdenv.isLinux) [ tezosClient ]
           ++ (with pkgs; [ niv ruby bc sphinx poetry entr nodePackages.live-server ])
           ++ spec.buildInputs
           ++ ocamlDeps pkgs
           ++ pythonDeps.buildInputs;
         shellHook = ''
           ${pythonDeps.shellHook}
         '';
       };
}
