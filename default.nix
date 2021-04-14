{ doCheck ? false }:

let
  sources = import ./nix/sources.nix { };

  pkgsHost = import sources.nixpkgs {};
  pkgsLinux = import sources.nixpkgs { system = "x86_64-linux"; };

  gitignoreNix = import sources."gitignore.nix" { lib = pkgsHost.lib; };

  ligoBinary =
    # This is a precompiled file, which is the ligo revision `c146c829b` compiled
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
  ];
in
{
  michelson =
    let pkgs = pkgsLinux;
    in pkgs.stdenv.mkDerivation {
         name = "huxian-michelson";
         buildInputs = [ ligoBinary ] ++ (with pkgs; [ ruby perl bc ]) ++ ocamlDeps pkgs;
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
  shell =
    let pkgs = pkgsHost;
    in pkgs.mkShell {
         name = "huxian-shell";
         buildInputs =
           # ligo does not compile on macos, also we don't want to
           # compile it in CI
           pkgs.lib.optionals (pkgsHost.stdenv.isLinux)
             [ ligoBinary
               (import "${sources.tezos-packaging}/nix" { }).binaries.tezos-client
             ]
           ++ [ pkgs.niv pkgs.perl pkgs.ruby pkgs.bc ]
           ++ ocamlDeps pkgs;

       };
}
