{ doCheck ? false, isCi ? false, skipLongTests ? false }:

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

in
{
  michelson =
    let pkgs = pkgsLinux;
    in pkgs.stdenv.mkDerivation {
         name = "huxian-michelson";
         buildInputs = [ ligoBinary ] ++ (with pkgs; [ ruby ]) ++ ocamlDeps pkgs;
         src =
           let filter =
             let ignored = gitignoreNix.gitignoreFilter ./.;
             in  path: type: ignored path type && builtins.baseNameOf path != "ligo";
           in pkgsHost.lib.cleanSourceWith {
                inherit filter;
                src = ./.;
                name = "huxian-source";
              };
         buildPhase = ''
           export HOME=$(mktemp -d)
           make build-ligo
         '';

         inherit doCheck;
         checkPhase =
          let tests = if skipLongTests then "test-main" else "test";
          in ''
            make build-ocaml
            make test ${tests}
          '';
         installPhase = ''
           mkdir -p $out
           cp generated/michelson/* $out
         '';
       };
  shell =
    let pkgs = pkgsHost;
    in pkgs.mkShell {
         name = "huxian-shell";
         buildInputs =
           # ligo does not compile on macos, also we don't want to
           # compile it in CI
           pkgs.lib.optionals (pkgsHost.stdenv.isLinux) [ ligoBinary ]
           ++ pkgs.lib.optionals (pkgsHost.stdenv.isLinux && !isCi) [ tezosClient ]
           ++ [ pkgs.niv pkgs.ruby pkgs.bc ]
           ++ ocamlDeps pkgs;

       };
}
