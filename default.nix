let
  sources = import ./nix/sources.nix { };

  pkgsHost = import sources.nixpkgs {};
  pkgsLinux = import sources.nixpkgs { system = "x86_64-linux"; };

  gitignoreNix = import sources."gitignore.nix" { inherit (pkgsHost.lib); };

  ligoBinary =
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
in
{
  michelson =
    let pkgs = pkgsLinux;
    in pkgs.stdenv.mkDerivation {
         name = "huxian-michelson";
         buildInputs = [ ligoBinary pkgs.ruby pkgs.ocamlPackages.ocp-indent pkgs.perl ];
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
         installPhase = ''
           mkdir -p $out
           cp generated/michelson/*.tz $out
         '';
       };
  shell =
    let pkgs = pkgsHost;
    in pkgs.mkShell {
         name = "huxian-shell";
         buildInputs =
           # ligo does not compile on macos, also we don't want to
           # compile it in CI
           pkgsHost.lib.optionals (pkgsHost.stdenv.isLinux)
             [ ligoBinary
             ]
           ++ [ pkgs.niv pkgs.perl pkgs.ruby ]
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
       };
}
