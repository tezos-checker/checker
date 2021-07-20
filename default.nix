{ doCheck ? false, e2eTestsHack ? false }:

let
  sources = import ./nix/sources.nix { };

  overlay = se: su: {
    poetry2nix = se.callPackage sources.poetry2nix {};
  };

  pkgsHost = import sources.nixpkgs { overlays = [ overlay ]; };
  pkgsLinux = import sources.nixpkgs { system = "x86_64-linux"; overlays = [ overlay ]; };

  gitignoreNix = import sources."gitignore.nix" { lib = pkgsHost.lib; };

  ligoBinary =
    # This is a precompiled file, which is the ligo revision `1d1cc2cae` compiled
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

  checkerSource =
    let filter =
      let ignored = gitignoreNix.gitignoreFilter ./.;
      in path: type: ignored path type && builtins.baseNameOf path != "ligo";
    in
    pkgsHost.lib.cleanSourceWith {
      inherit filter;
      src = ./.;
      name = "checker-source";
    };
in
rec
{
  michelson =
    let pkgs = pkgsLinux;
    in
    pkgs.stdenv.mkDerivation {
      name = "checker-michelson";
      buildInputs = [ ligoBinary ] ++ (with pkgs; [ ruby ]) ++ ocamlDeps pkgs;
      src = checkerSource;
      # On E2E tests, we are using a patched version of checker to be able to experiment
      # with index changes without having to wait for the protected index to catch up.
      # It's a hack, and shouldn't be used for anything else than the tests.
      patchPhase = pkgs.lib.optional e2eTestsHack ''
        set -x
        cat ${./patches/e2e-tests-hack.patch} | patch -p1
        set +x
      '';
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
    in
    pkgs.stdenv.mkDerivation {
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
    let
      pkgs = pkgsHost;
      pythonDeps = import ./nix/python.nix { inherit pkgs; };
    in
    pkgs.mkShell {
      name = "checker-shell";
      buildInputs =
        # ligo does not compile on macos, also we don't want to
        # compile it in CI
        pkgs.lib.optionals (pkgs.stdenv.isLinux) [ ligoBinary ]
        ++ pkgs.lib.optionals (!(pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64)) [ pkgs.niv ]
        ++ (with pkgs; [ ruby bc sphinx poetry entr nodePackages.live-server fd python3Packages.black nixpkgs-fmt ])
        ++ spec.buildInputs
        ++ ocamlDeps pkgs
        ++ pythonDeps.buildInputs;
      shellHook = ''
        ${pythonDeps.shellHook}
      '';
    };
}
